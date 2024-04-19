{-# LANGUAGE GADTs #-}

module RealLanguage.Elaboration where

import RealLanguage.Operational
import RealLanguage.Syntax

type Declarations = [(String,Declaration)]
data Declaration
    = TypeNameExists
    | ConNameExists ConSig
    | TermNameExists Type (Maybe Term)
    deriving (Show)

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
    CheckTerm :: Declarations -> Type -> Term -> Goal ()

    -- [ D ; G !- M syn A true ]
    -- in: D, G, M. out: A
    SynthesizeTerm :: Declarations -> Term -> Goal Type

data ElabError
    = TypeNameAlreadyUsed TypeName
    | ConstructorNameAlreadyUsed ConName
    | InvalidType Type
    | TermNameAlreadyUsed TermName
    | TermNameNotDeclared TermName
    deriving (Show)

type Elab = Operational ElabInstr

throw :: ElabError -> Elab r
throw = exec . Throw

goal :: Goal r -> Elab r
goal g = exec (Prove g)

runElab :: Elab r -> Either ElabError r
runElab (Ret x) = Right x
runElab (Instr i k) = case i of
    Throw err -> Left err
    Prove g -> runElab (prove g >>= k)

prove :: Goal r -> Elab r
prove (ProgramValid d p) = programValid d p
prove (StatementValid d s) = statementValid d s
prove (TypeValid d a) = typeValid d a
prove (CheckTerm d a m) = checkTerm d a m
prove (SynthesizeTerm d m) = synthesizeTerm d m




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
        goal (StatementValid d s)
        goal (ProgramValid d (Program ss)) 


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
            goal (CheckTerm d a m)
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
          -> Type
          -> Term
          -> Elab ()
checkTerm d a m = _



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
synthesizeTerm :: Declarations
               -> Term
               -> Elab Type
synthesizeTerm d m = _