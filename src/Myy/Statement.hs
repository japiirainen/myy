{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Defines the Myy Statement type, which can either be a bare expression
--   or a global variable assignment

module Myy.Statement
  ( Statement(..)
  ) where

import           Myy.Unchecked
import           Text.PrettyPrint.ANSI.Leijen

-- | A statement can either be a bare expression, which will be evaluated,
--   or an assignment to a global variable.
data Statement
  = BareExp UExp
  | NewGlobal String UExp

instance Pretty Statement where
  pretty (BareExp exp) = pretty exp
  pretty (NewGlobal v exp) = text v <+> char '=' <+> pretty exp
