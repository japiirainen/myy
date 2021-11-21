{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Utility definitions for testing myy

module Util
  ( module Test.Tasty
  , testCase
  , (@?=)
  , (@=?)
  , (@?)
  ) where

import           Myy.Util
import           Test.Tasty
import           Test.Tasty.HUnit             (Assertion, testCase, (@?))
import           Text.PrettyPrint.ANSI.Leijen

import           Data.Function
import           Language.Haskell.TH
import           Text.Parsec                  (ParseError)

prettyError :: Pretty a => a -> a -> String
prettyError exp act = render $ text "Expected" <+> squotes (pretty exp) <> semi <+>
                                text "got" <+> squotes (pretty act)

(@?=) :: (Eq a, Pretty a) => a -> a -> Assertion
act @?= exp = (act == exp) @? prettyError exp act

(@=?) :: (Eq a, Pretty a) => a -> a -> Assertion
exp @=? act = (act == exp) @? prettyError exp act

$( do decs <- reifyInstances ''Eq [ConT ''ParseError]
      case decs of
        [] -> fmap (:[]) $
              instanceD (return []) (appT (conT ''Eq) (conT ''ParseError))
                        [ valD (varP '(==)) (normalB [| (==) `on` show |]) [] ]
        _  -> return [] )

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x)  = text "Left" <+> pretty x
  pretty (Right x) = text "Right" <+> pretty x
