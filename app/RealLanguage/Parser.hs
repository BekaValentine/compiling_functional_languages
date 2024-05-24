module RealLanguage.Parser where

import Control.Monad (guard)
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified RealLanguage.Surface as S
import qualified RealLanguage.Names as S
import qualified RealLanguage.Types as S

parse :: String -> Parser a -> String -> Either String a
parse path parser input =
    case MP.parse parser path input of
        Left err -> Left (errorBundlePretty err)
        Right x -> Right x

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

tok :: String -> Parser String
tok = L.symbol space

tok_ :: String -> Parser ()
tok_ s =
    do
        _ <- tok s
        return ()

infixl 9 >>=?

(>>=?) :: Parser a -> (a -> Parser a) -> Parser a
p >>=? f =
  do x <- p
     option x (f x)

keywords :: [String]
keywords =
    [ "data"
    , "con"
    , "term"
    , "case"
    , "of"
    , ";"
    , ":"
    , "~>"
    , "("
    , ")"
    , ","
    , "->"
    , "="
    , "\\"
    , "|"
    ]

program :: Parser S.Program
program = S.Program <$> many statement

statement :: Parser S.Statement
statement
      = dataDecl
    <|> conDecl
    <|> tmDecl
    <|> tmDef

dataDecl :: Parser S.Statement
dataDecl =
    do
        try $ tok_ "data"
        tn <- typeName
        tok_ ";"
        return $ S.DataDecl tn

typeName :: Parser S.TypeName
typeName =
    S.TypeName <$> lexeme (do
        c <- upperChar
        cs <- many alphaNumChar
        let n = c:cs
        guard (not (elem n keywords))
        return $ n)

conDecl :: Parser S.Statement
conDecl =
    do
        try $ tok_ "con"
        cn <- conName
        tok_ ":"
        csig <- consig
        tok_ ";"
        return $ S.ConDecl cn csig

conName :: Parser S.ConName
conName =
    S.ConName <$> lexeme (do
        c <- upperChar
        cs <- many alphaNumChar
        let n = c:cs
        guard (not (elem n keywords))
        return $ n)

consig :: Parser S.ConSig
consig =
    do
        tok_ "("
        as <- sepBy typeP (tok_ ",")
        tok_ ")"
        tok_ "~>"
        b <- typeP
        return $ S.ConSig as b

tmDecl :: Parser S.Statement
tmDecl =
    do
        tmn <- try $ do
            tok_ "term"
            tmn <- termName
            tok_ ":"
            return tmn
        a <- typeP
        tok_ ";"
        return $ S.TermDecl tmn a

termName :: Parser S.TermName
termName =
    S.TermName <$> lexeme (do
        _ <- char '$'
        c <- lowerChar
        cs <- many alphaNumChar
        let n = c:cs
        guard (not (elem n keywords))
        return $ n)

tmDef :: Parser S.Statement
tmDef =
    do
        tmn <- try $ do
            tok_ "term"
            tmn <- termName
            tok_ "="
            return tmn
        m <- term
        tok_ ";"
        return $ S.TermDef tmn m

typeP :: Parser S.Type
typeP
      = (constructedType <|> parenthesizedType)
            >>=? functionTypeSuffix


constructedType :: Parser S.Type
constructedType =
    do
        tn <- try $ typeName
        return $ S.ConTy tn

-- functionType :: Parser S.Type
-- functionType =
--     do
--         a <- functionArgType
--         tok_ "->"
--         b <- typeP
--         return $ S.FunTy a b

functionTypeSuffix :: S.Type -> Parser S.Type
functionTypeSuffix a =
    do
        bs <- some $ do
            tok_ "->"
            typeP
        return $ foldl S.FunTy a bs

functionArgType :: Parser S.Type
functionArgType
      = constructedType
    <|> parenthesizedType

parenthesizedType :: Parser S.Type
parenthesizedType =
    do
        try $ tok_ "("
        a <- typeP
        tok_ ")"
        return a

term :: Parser S.Term
term
      = caseTerm
    <|> lambda
    <|> (variable
            <|> declaredTerm
            <|> constructedTerm
            <|> parenthesizedTerm)
        >>=? applicationSuffix
        >>=? annotationSuffix

variable :: Parser S.Term
variable =
    do
        vn <- try variableName
        return $ S.Var vn

variableName :: Parser S.VarName
variableName =
    S.VarName <$> lexeme (do
        c <- lowerChar
        cs <- many alphaNumChar
        let n = c:cs
        guard (not (elem n keywords))
        return $ n)

declaredTerm :: Parser S.Term
declaredTerm =
    do
        tmn <- try termName
        return $ S.DefVar tmn

constructedTerm :: Parser S.Term
constructedTerm =
    do
        cn <- try conName
        tok_ "("
        ms <- sepBy term (tok_ ",") 
        tok_ ")"
        return $ S.Con cn ms

caseTerm :: Parser S.Term
caseTerm =
    do
        try $ tok_ "case"
        ms <- sepBy1 term (tok_ "|")
        tok_ "of"
        cls <- some clause
        return $ S.Case ms cls

clause :: Parser S.Clause
clause =
    do
        tok_ "|"
        ps <- sepBy1 pat (tok_ "|")
        tok_ "->"
        m <- term
        return $ S.Clause ps m

pat :: Parser S.Pattern
pat
      = varPat
    <|> conPat

varPat :: Parser S.Pattern
varPat =
    do
        vn <- try variableName
        return $ S.VarPat vn

conPat :: Parser S.Pattern
conPat =
    do
        cn <- try conName
        tok_ "("
        ps <- sepBy pat (tok_ ",")
        tok_ ")"
        return $ S.ConPat cn ps

lambda :: Parser S.Term
lambda =
    do
        try $ tok_ "\\"
        vn <- variableName
        tok_ "->"
        m <- term
        return $ S.Lambda vn m

-- application :: Parser S.Term
-- application =
--     do
--         m <- applicationLeft
--         n <- applicationRight
--         return $ S.Apply m n

applicationSuffix :: S.Term -> Parser S.Term
applicationSuffix f =
    do
        ms <- some applicationRight
        return $ foldl S.Apply f ms

-- applicationLeft :: Parser S.Term
-- applicationLeft
--       = variable
--     <|> declaredTerm
--     <|> constructedTerm
--     <|> application
--     <|> parenthesizedTerm

applicationRight :: Parser S.Term
applicationRight
      = variable
    <|> declaredTerm
    <|> constructedTerm
    <|> parenthesizedTerm

-- annotation :: Parser S.Term
-- annotation =
--     do
--         m <- annotationTerm
--         tok_ ":"
--         a <- typeP
--         return $ S.Ann m a

annotationSuffix :: S.Term -> Parser S.Term
annotationSuffix m =
    do
        tok_ ":"
        a <- typeP
        return $ S.Ann m a

-- annotationTerm :: Parser S.Term
-- annotationTerm
--       = variable
--     <|> declcasearedTerm
--     <|> constructedTerm
--     <|> application
--     <|> annotation
--     <|> parenthesizedTerm

parenthesizedTerm :: Parser S.Term
parenthesizedTerm =
    do
        try $ tok_ "("
        m <- term
        tok_ ")"
        return m