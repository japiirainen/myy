module Myy.Parser
    ( parseExpr
    , parseModule
    ) where

import           Text.Parsec
import           Text.Parsec.Text.Lazy (Parser)

import qualified Data.Text.Lazy        as L
import qualified Text.Parsec.Expr      as Ex
import qualified Text.Parsec.Token     as Tok

import           Myy.Lexer
import           Myy.Syntax

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = Var <$> identifier

number :: Parser Expr
number = do
    Lit . LInt . fromInteger <$> integer

bool :: Parser Expr
bool = (reserved "true" >> return (Lit (LBool True)))
    <|> (reserved "false" >> return (Lit (LBool False)))

fix :: Parser Expr
fix = do
    reservedOp "fix"
    Fix <$> expr

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args

letin :: Parser Expr
letin = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let x e1 e2

letrecin :: Parser Expr
letrecin = do
    reserved "let"
    reserved "rec"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let x e1 e2

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return $ If cond e2 e3

aexp :: Parser Expr
aexp =
    parens expr
    <|> bool
    <|> number
    <|> ifthen
    <|> fix
    <|> try letrecin
    <|> letin
    <|> lambda
    <|> variable

term :: Parser Expr
term = aexp >>= \x ->
    (many1 aexp >>= \xs -> return (foldl App x xs))
    <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
          [ infixOp "*" (Op Mul) Ex.AssocLeft ]
        , [ infixOp "+" (Op Add) Ex.AssocLeft
          , infixOp "-" (Op Sub) Ex.AssocLeft
          ]
        , [ infixOp "==" (Op Eql) Ex.AssocLeft]
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
    reserved "let"
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- expr
    return (name, foldr Lam body args)

letrecdecl :: Parser Binding
letrecdecl = do
    reserved "let"
    reserved "rec"
    name <- identifier
    args <- many identifier
    reservedOp "="
    body <- expr
    return (name, Fix $ foldr Lam body args)

val :: Parser Binding
val = do
    ex <- expr
    return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
    x <- decl
    optional semi
    return x

modl :: Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule :: FilePath -> L.Text -> Either ParseError [Binding]
parseModule = parse (contents modl)
