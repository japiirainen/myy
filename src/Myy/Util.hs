{-# OPTIONS_GHC -Wno-orphans #-}
{- Utility exports (and re-exports) for myy. This module is meant to
   be internal. Do not import it if you are not part of the myy package!
-}

module Myy.Util
    ( render
    , toSimpleDoc
    , maybeParens
    , ($$)
    , Prec
    , topPrec
    , stripWhitespace
    , nthDefault
    , (:~:)(..)
    , ignore
    ) where

import           Text.PrettyPrint.ANSI.Leijen as Pretty

import           Data.Char
import           Data.List

import           Data.Type.Equality
import           Text.Parsec                  (ParseError)

-- | Like `Data.Functor.Void`
ignore :: Functor f => f a -> f ()
ignore = (() <$)

instance Pretty ParseError where
    pretty = text . show

-- | More perspicuous synonym for operator precedence
type Prec = Rational

-- | Precedence for top-level printing
topPrec :: Prec
topPrec = 0

-- | Convert a `Doc` to a `String`
render :: Doc -> String
render = flip displayS "" . toSimpleDoc

-- | Convert a `Doc` to a `SimpleDoc` for further rendering
toSimpleDoc :: Doc -> SimpleDoc
toSimpleDoc = renderPretty 1.0 78

-- | Enclose a `Doc` in parens if the flag is `True`
maybeParens :: Bool -> Doc -> Doc
maybeParens True  = parens
maybeParens False = id

-- | Synonym for `Pretty.<$>`
($$) :: Doc -> Doc -> Doc
d1 $$ d2 = d1 Pretty.<$> d2

-- | (Inefficiently) strip whitespace from a string
stripWhitespace :: String -> String
stripWhitespace = dropWhile isSpace . dropWhileEnd isSpace

-- | Pluck out the nth item from a list, or use a default if the list
--   is too short
nthDefault :: a -> Int -> [a] -> a
nthDefault _ 0 (x:_)    = x
nthDefault def n (_:xs) = nthDefault def (n-1) xs
nthDefault def _ []     = def
