{-# LANGUAGE LambdaCase #-}
-- | Parses tokens into the un-type-checked AST. "Parsing", in myy,
--   also includes name resolution. This all might
--   conceivably be done in a later pass, but there doesn't seem to be
--   an incentive to do so

module Myy.Parse
  ( parseStmtsG
  , parseStmts
  , parseStmtG
  , parseStmt
  , parseExpG
  , parseExp
  ) where

import           Myy.Monad
import           Myy.Statement
import           Myy.Token
import           Myy.Type
import           Myy.Unchecked
import           Myy.Util

import           Text.Parsec.Combinator
import           Text.Parsec.Pos
import           Text.Parsec.Prim             as Parsec hiding (parse)

import           Control.Applicative
import           Control.Arrow                as Arrow (left)
import           Control.Monad.Reader
import           Data.List                    as List
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | Parse a sequence of semicolon-separated statements, aborting with
--   an error upon failure
parseStmtsG :: [LToken] -> MyyE [Statement]
parseStmtsG = eitherToMyyE . parseStmts

-- | Parse a sequence of semicolon-separated statements
parseStmts :: [LToken] -> Either String [Statement]
parseStmts = parse stmts

-- | Parse a `Statement`, aborting with an error upon failure
parseStmtG :: [LToken] -> MyyE Statement
parseStmtG = eitherToMyyE . parseStmt

-- | Parse a `Statement`
parseStmt :: [LToken] -> Either String Statement
parseStmt = parse stmt

-- | Parse a `UExp`, aborting with an error upon failure
parseExpG :: [LToken] -> MyyE UExp
parseExpG = eitherToMyyE . parseExp

-- | Parse a `UExp`
parseExp :: [LToken] -> Either String UExp
parseExp = parse expr

parse :: Parser a -> [LToken] -> Either String a
parse p toks = Arrow.left show $
  runReader (runParserT (p <* eof) () "" toks) []

-- Plumbing

-- the "state" is a list of bound names. searching a bound name in the list
-- gives you the correct deBrujin index
type Parser = ParsecT [LToken] () (Reader [String])

-- | Bind a name over an expression
bind :: String -> Parser a -> Parser a
bind bound_var = local (bound_var :)

-- | Parse the given nullary token
tok :: Token -> Parser ()
tok t = tokenPrim (render . pretty) next_pos (guard . (== t) . unLoc)

-- | Parse the given unary token
tok' :: (Token -> Maybe thing) -> Parser thing
tok' matcher = tokenPrim (render . pretty) next_pos (matcher . unLoc)

-- | Parse one of a set of `ArithOp`s
arith_op :: [UArithOp] -> Parser UArithOp
arith_op ops = tokenPrim (render . pretty) next_pos
                           (\case L _ (ArithOp op) | op `elem` ops -> Just op
                                  _                                -> Nothing)

next_pos :: SourcePos
         -- ^ position of the current token
         -> LToken
         -- ^ current token
         -> [LToken]
         -- ^ remaining tokens
         -> SourcePos
         -- ^ location of the next token
next_pos pos _ []          = pos
next_pos _ _ (L pos _ : _) = pos

-- Real work

stmts :: Parser [Statement]
stmts = stmt `sepEndBy` tok Semi

stmt :: Parser Statement
stmt = choice [ try $ NewGlobal <$> tok' unName <* tok Assign <*> expr
              , BareExp <$> expr
              ]

expr :: Parser UExp
expr = choice [ lam
              , cond
              , int_exp `chainl1` bool_op
              ]

int_exp :: Parser UExp
int_exp = term `chainl1` add_op

term :: Parser UExp
term = apps `chainl1` mul_op

apps :: Parser UExp
apps = choice [ UFix <$ tok FixT <*> expr
              , List.foldl1 UApp <$> some factor
              ]

factor :: Parser UExp
factor= choice [ between (tok LParen) (tok RParen) expr
              , UIntE <$> tok' unInt
              , UBoolE <$> tok' unBool
              , var
              ]

lam :: Parser UExp
lam = do
  tok Lambda
  bound_var <- tok' unName
  tok Colon
  typ <- ty
  tok Dot
  e <- bind bound_var expr
  return (ULam typ e)

cond :: Parser UExp
cond = UCond <$ tok If <*> expr <* tok Then <*> expr <* tok Else <*> expr

var :: Parser UExp
var = do
  n <- tok' unName
  m_index <- asks (elemIndex n)
  case m_index of
    Nothing -> return (UGlobal n)
    Just i  -> return (UVar i)

ty :: Parser Ty
ty = chainr1 arg_ty (Arr <$ tok Arrow)

arg_ty :: Parser Ty
arg_ty = choice [ between (tok LParen) (tok RParen) ty
                , tycon
                ]

tycon :: Parser Ty
tycon = do
  n <- tok' unName
  case readTyCon n of
    Nothing -> unexpected $ render $ text "type" <+> squotes (text n)
    Just t  -> return t

add_op, mul_op, bool_op :: Parser (UExp -> UExp -> UExp)
add_op = mk_op <$> arith_op [uPlus, uMinus]
mul_op = mk_op <$> arith_op [uTimes, uDivide, uMod]
bool_op = mk_op <$> arith_op [uLess, uLessE, uGreater, uGreaterE, uEquals]

mk_op :: UArithOp -> UExp -> UExp -> UExp
mk_op op e1 e2 = UArith e1 op e2
