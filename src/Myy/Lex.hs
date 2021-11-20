-- Lexes a Myy program string into a sequence of `Tokens`

module Myy.Lex
  ( lex
  , lexG
  ) where

import           Prelude                hiding (lex)

import           Myy.Monad
import           Myy.Token
import           Myy.Util

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Language
import           Text.Parsec.Prim       (Parsec, getPosition, parse, try)
import           Text.Parsec.Token      as Parsec

import           Control.Applicative
import           Control.Arrow          as Arrow
import           Data.Maybe

type Lexer = Parsec String ()

-- Utility
string_ :: String -> Lexer ()
string_ = ignore . string

-- | Lex some program text into a list of `LToken's`, aborting upon failure
lexG :: String -> MyyE [LToken]
lexG = eitherToMyyE . lex

-- | Lex some program text into a list of `LToken's`
lex :: String -> Either String [LToken]
lex = Arrow.left show . parse lexer ""

-- | Overall lexer
lexer :: Lexer [LToken]
lexer = (catMaybes <$> many lexer1_ws) <* eof

lexer1_ws :: Lexer (Maybe LToken)
lexer1_ws =
  (Nothing <$ whitespace)
  <|>
  (Just <$> lexer1)

-- | Lex some whitespace
whitespace :: Lexer ()
whitespace = choice
                [ ignore $ some space
                , block_comment
                , line_comment
                ]

-- | Lex a @{- ... -}@ comment (perhaps nested); consumes no input
--   if the target doesn't start with @{-@.
block_comment :: Lexer ()
block_comment = do
  try $ string_ "{-"
  comment_body

-- | Lex a block comment, without the opening "{-"
comment_body :: Lexer ()
comment_body = choice
                  [ block_comment *> comment_body
                  , try $ string_ "-}"
                  , anyChar *> comment_body
                  ]

-- | Lex a line comment
line_comment :: Lexer ()
line_comment = do
  try $ string_ "--"
  ignore $ manyTill anyChar (eof <|> ignore newline)

-- | Lex one token
lexer1 :: Lexer LToken
lexer1 = do
  pos <- getPosition
  L pos <$> choice [ symbolic
                   , word_token
                   , Int . fromInteger <$> Parsec.natural haskell
                   ]

-- | Lex one non-alphanumeric token
symbolic :: Lexer Token
symbolic = choice
              [ LParen <$ char '('
              , RParen <$ char ')'
              , Lambda <$ char '\\'
              , Dot <$ char '.'
              , Arrow <$ try (string "->")
              , Colon <$ char ':'
              , ArithOp <$> arith_op
              , Assign <$ char '='
              , Semi <$ char ';'
              ]

-- | Lex one arithmetic operator
arith_op :: Lexer UArithOp
arith_op = choice
              [ UArithOp Plus <$ char '+'
              , UArithOp Minus <$ char '-'
              , UArithOp Times <$ char '*'
              , UArithOp Divide <$ char '/'
              , UArithOp Mod <$ char '%'
              , UArithOp LessE <$ try (string "<=")
              , UArithOp Less <$ char '<'
              , UArithOp GreaterE <$ try (string ">=")
              , UArithOp Greater <$ char '>'
              , UArithOp Equals <$ try (string "==")
              ]

-- | Lex one alphanumeric token
word_token :: Lexer Token
word_token = to_token <$> word
  where
    to_token "true"  = Bool True
    to_token "false" = Bool False
    to_token "if"    = If
    to_token "then"  = Then
    to_token "else"  = Else
    to_token "fix"   = FixT
    to_token other   = Name other

-- | Lex one word
word :: Lexer String
word = (:) <$> (letter <|> char '_') <*>
                many (alphaNum <|> char '_')
