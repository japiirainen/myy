-- Lexes a Myy program string into a sequence of `Tokens`

module Myy.Lex where

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
