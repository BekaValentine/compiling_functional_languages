{-# LANGUAGE GADTs #-}

module RealLanguage.Elaboration where

import Control.Monad (when, zipWithM, zipWithM_)
import Data.List ((\\), nub)

import RealLanguage.Names
import RealLanguage.Operational
import RealLanguage.Surface
import RealLanguage.Types
import qualified RealLanguage.Core as Core

type Declarations = [(String,Declaration)]
data Declaration
    = TypeNameExists
    | ConNameExists ConSig
    | TermNameExists Type (Maybe Core.Term)
    deriving (Show)

type Context = [(VarName, Type)]

data ElabInstr r
    = Throw ElabError
    | Prove (Goal r)

data Goal r where
    -- [ D !- Prog -! D' ]
    -- in: D, Prog. out: D'
    ProgramValid :: Declarations
                 -> Program
                 -> Goal Declarations

    -- [ D !- Stmt -! D' ]
    -- in: D, Stmt. out: D'
    StatementValid :: Declarations
                   -> Statement
                   -> Goal Declarations

    -- [ D !- A type ]
    -- in: D, A
    TypeValid :: Declarations -> Type -> Goal ()

    -- [ D ; G !- A true chk M ]
    -- in: D, G, A, M
    CheckTerm :: Declarations
              -> Context
              -> Type
              -> Term
              -> Goal Core.Term

    -- [ D ; G !- M syn A true ]
    -- in: D, G, M. out: A
    SynthesizeTerm :: Declarations
                   -> Context
                   -> Term
                   -> Goal (Core.Term, Type)

    -- [ D ; G !- Cl : A* clause B ]
    -- in: D, G, Cl, A*. out: B.
    CheckClause :: Declarations
                -> Context
                -> [Type]
                -> Clause
                -> Goal (Core.Clause, Type)
    
    -- [ D ; G !- P : A pattern G' ]
    -- in: D, G, P, A. out: G'.
    CheckPattern :: Declarations
                 -> Context
                 -> Pattern
                 -> Type
                 -> Goal (Core.Pattern, Context)

data ElabError
    = TypeNameAlreadyUsed TypeName
    | ConstructorNameAlreadyUsed ConName
    | InvalidType Type
    | TermNameAlreadyUsed TermName
    | TermNameNotDeclared TermName
    | DirectionChangeTypeMismatch Type Type Term
    | NameNotAConName String
    | ConNameNotDeclared String
    | ConstructorMismatchReturnType Type Type Term
    | ConstructorMismatchNumberArguments Term Int Int
    | ApplicationNonFunctionType Term Type
    | CaseStatementNoClauses Term
    | MismatchedClauseTypes Term
    | UnknownVariable VarName
    | CannotSynthesize Term
    | MismatchedPatternLength Clause Int Int
    | ConstructorMismatchNumberArgumentsPattern Pattern Int Int
    | ConstructorMismatchReturnTypePattern Type Type Pattern
    | RepeatedPatternVariables Pattern [VarName]
    deriving (Show)

type Elab = Operational ElabInstr

throw :: ElabError -> Elab r
throw = exec . Throw

goal :: Goal r -> Elab r
goal = exec . Prove

runElab :: Elab r -> Either ElabError r
runElab (Ret x) = Right x
runElab (Instr i k) = case i of
    Throw err -> Left err
    Prove g -> runElab (decompose g >>= k)
    
    where
        -- `decompose` is used to decompose a goal into a means of elaborating
        -- the goal. It's defined in terms of some more isolated little
        -- functions for each kind of goal.
        decompose :: Goal r -> Elab r
        decompose (ProgramValid d p)      = programValid d p
        decompose (StatementValid d s)    = statementValid d s
        decompose (TypeValid d a)         = typeValid d a
        decompose (CheckTerm d g a m)     = checkTerm d g a m
        decompose (SynthesizeTerm d g m)  = synthesizeTerm d g m
        decompose (CheckClause d g cl as) = checkClause d g cl as
        decompose (CheckPattern d g p a)  = checkPattern d g p a




-- [ D !- Prog -! D' ]
-- in: D, Prog. out: D'
--
-- Given declarations D, program P is valid and gives rise to declarations D'
--
-- ------------ empty program
-- D !- "" -! D
--
-- D !- Stmt -! D'
-- D' !- Prog -! D''
-- --------------------- non-empty program
-- D !- Stmt Prog -! D''
programValid :: Declarations
             -> Program
             -> Elab Declarations
programValid d (Program []) =
    return d
programValid d (Program (s:ss)) =
    do
        d' <- goal (StatementValid d s)
        goal (ProgramValid d' (Program ss)) 


-- [ D !- Stmt -! D' ] 
-- in: D, Stmt. out: D'
--
-- Given declarations D, statement Stmt is valid and gives rise to declarations D'
--
-- D does not contain TN 
-- ---------------------------- data declaration
-- D !- data TN; -! D, TN tycon
--
-- D does not contain CN
-- D !- A_i type
-- D !- B type
-- ----------------------------------------------- constructor declaration
-- D !- con CN : (A*) ~> B; -! D, CN con (A*) ~> B
--
-- D does not contain TmN
-- D !- A type
-- -------------------------------- term declaration
-- D !- term TmN : A; -! D, TmN : A
--
-- D contains TmN : A
-- D ; <> !- A true chk M
-- -------------------------------- term definition
-- D !- term TmN = M; -! D, TmN = M
statementValid :: Declarations
               -> Statement
               -> Elab Declarations

statementValid d (DataDecl (TypeName tn)) =
    case lookup tn d of
        Just _ -> throw (TypeNameAlreadyUsed (TypeName tn))
        Nothing -> return ((tn, TypeNameExists) : d)

statementValid d (ConDecl (ConName cn) csig@(ConSig as b)) =
    case lookup cn d of
        Just _ -> throw (ConstructorNameAlreadyUsed (ConName cn))
        _ -> do
            mapM_ (goal . TypeValid d) as
            goal (TypeValid d b)
            return ((cn, ConNameExists csig):d)

statementValid d (TermDecl (TermName tmn) a) =
    case lookup tmn d of
        Just _ -> throw (TermNameAlreadyUsed (TermName tmn))
        Nothing -> do
            goal (TypeValid d a)
            return ((tmn, TermNameExists a Nothing):d)

statementValid d (TermDef (TermName tmn) m) =
    case lookup tmn d of
        Just (TermNameExists a Nothing) -> do
            goal (CheckTerm d [] a m)
            return ((tmn, TermNameExists a (Just m)):d)
        Nothing -> throw (TermNameNotDeclared (TermName tmn))
        Just _ -> throw (TermNameAlreadyUsed (TermName tmn))



-- [ D !- A type ]
-- in: D, A
--
-- Given declarations D, A is a type.
--
-- D contains TN tycon
-- -------------------
-- D !- TN type
--
-- D !- A type
-- D !- B type
-- ---------------- function type
-- D !- A -> B type
typeValid :: Declarations -> Type -> Elab ()
typeValid d a@(ConTy (TypeName tn)) =
    case lookup tn d of
        Just TypeNameExists -> return ()
        _ -> throw (InvalidType a)
typeValid d (FunTy b c) =
    do
        goal (TypeValid d b)
        goal (TypeValid d c)


-- [ D ; G !- A true chk M ]
-- in: D, G, A, M
--
-- Given declarations D and variables G, the type A true checks the term M.
--
-- D contains CN con (B*) ~> A
-- |M*| = |B*|
-- D ; G !- B_i true chk M_i
-- --------------------------- constructors
-- D ; G !- A true chk CN(M*)
--
-- D ; G, x : A !- B true chk M
-- -------------------------------- lambdas
-- G ; G !- A -> B true chk \x -> M
--
-- D ; G !- M syn B true
-- A = B
-- --------------------- direction change
-- D ; G !- A true chk M
checkTerm :: Declarations
          -> Context
          -> Type
          -> Term
          -> Elab Core.Term
checkTerm d g a m@(Con (ConName cn) ms) =
    case lookup cn d of
        Just (ConNameExists (ConSig bs a')) ->
            if a /= a'
            then throw (ConstructorMismatchReturnType a a' m)
            else if length ms /= length bs
            then throw (ConstructorMismatchNumberArguments m (length ms) (length bs))
            else zipWithM_
                   (\bi mi -> goal (CheckTerm d g bi mi))
                   bs
                   ms
        Just _ -> throw (NameNotAConName cn)
        Nothing -> throw (ConNameNotDeclared cn)
checkTerm d g (FunTy a b) (Lambda x m) =
    goal (CheckTerm d ((x,a):g) b m)
checkTerm d g a m =
    do
        b <- goal (SynthesizeTerm d g m)
        when (a /= b) $ throw (DirectionChangeTypeMismatch a b m)


-- [ D ; G !- M syn A true ]
-- in: D, G, M. out: A
--
-- Given declarations D and variables G, the term M synthesizes the type A true.
--
-- D ; G !- M_i syn A_i true
-- D ; G !- Cl_j : A* clause B
-- ---------------------------------- case
-- D ; G !- case M* of Cl* syn B true
--
-- D ; G !- M syn A -> B true
-- D ; G !- A true chk N 
-- -------------------------- application
-- D ; G !- M N syn B true
--
-- D !- A type
-- D ; G !- A true chk M
-- --------------------------- annotation
-- D ; G !- (M : A) syn A true
--
-- D contains TmN : A
-- ----------------------- term name
-- D ; G !- TmN syn A true
synthesizeTerm :: Declarations
               -> Context
               -> Term
               -> Elab (Core.Term, Type)

synthesizeTerm d g m@(Con (ConName cn) ms) =
    case lookup cn d of
        Just (ConNameExists (ConSig bs a)) ->
            if length ms /= length bs
            then throw (ConstructorMismatchNumberArguments m (length ms) (length bs))
            else do
                zipWithM_
                   (\bi mi -> goal (CheckTerm d g bi mi))
                   bs
                   ms
                return a
        Just _ -> throw (NameNotAConName cn)
        Nothing -> throw (ConNameNotDeclared cn)

synthesizeTerm d g m@(Case ms cls) =
    case cls of
        [] -> throw (CaseStatementNoClauses m)
        _ -> do
            as <- mapM (goal . SynthesizeTerm d g) ms
            bs <- mapM (goal . CheckClause d g as) cls
            let b = head bs
            if (any (b /=) bs)
                then throw (MismatchedClauseTypes m)
                else return b


synthesizeTerm d g m'@(Apply m n) =
    do
        t <- goal (SynthesizeTerm d g m)
        case t of
            FunTy a b ->
                do
                    goal (CheckTerm d g a n)
                    return b
            _ -> throw (ApplicationNonFunctionType m' t)

synthesizeTerm d g (Ann m a) =
    do
        goal (TypeValid d a)
        goal (CheckTerm d g a m)
        return a

synthesizeTerm d g (Var vn) =
    case lookup vn g of
        Just a -> return a
        Nothing -> throw (UnknownVariable vn)

synthesizeTerm d g (DefVar (TermName tn)) =
    case lookup tn d of
        Just (TermNameExists a _) -> return a
        Nothing -> throw (TermNameNotDeclared (TermName tn))

synthesizeTerm d g m =
    throw (CannotSynthesize m)


-- [ D ; G !- Cl : A* clause B ]
-- in: D, G, Cl, A*. out: B.
--
-- |P*| = |A*|
-- D ; G !- P_i : A_i pattern G'
-- D ; G, G' !- N syn B true
-- ------------------------------ clause
-- D ; G !- P* -> N : A* clause B

checkClause :: Declarations
            -> Context
            -> [Type]
            -> Clause
            -> Elab (Core.Clause, Type)
checkClause d g as cl@(Clause ps n) =
    if length ps /= length as
    then throw (MismatchedPatternLength cl (length ps) (length as))
    else do
        gs' <- zipWithM (\pi ai -> goal (CheckPattern d g pi ai)) ps as 
        goal (SynthesizeTerm d (g ++ concat gs') n)


-- [ D ; G !- P : A pattern G' ]
-- in: D, G, P, A. out: G'.
--
-- D contains CN consig B* ~> A'
-- |P*| = |B*|
-- A' = A
-- P* distinct vars
-- D ; G !- P_i : B_i pattern G'_i
-- ------------------------------- conpat
-- D ; G !- CN(P*) : A pattern G'*
--
-- ------------------------------
-- D ; G !- vn : A pattern vn : A

checkPattern :: Declarations
             -> Context
             -> Pattern
             -> Type
             -> Elab (Core.Pattern, Context)

checkPattern d g p@(ConPat (ConName cn) ps) a =
    case lookup cn d of
        Just (ConNameExists (ConSig bs a')) ->
            if length ps /= length bs
            then throw (ConstructorMismatchNumberArgumentsPattern p (length ps) (length bs))
            else if a /= a'
            then throw (ConstructorMismatchReturnTypePattern a a' p)
            else let vs = ps >>= patternVars
                     fvs = nub vs
            in if vs /= fvs
            then throw (RepeatedPatternVariables p (vs \\ fvs))
            else do
                gs' <- zipWithM (\pi bi -> goal (CheckPattern d g pi bi)) ps bs
                return (concat gs')
        Just _ -> throw (NameNotAConName cn)
        Nothing -> throw (ConNameNotDeclared cn)

checkPattern d g (VarPat vn) a =
    return [(vn, a)]