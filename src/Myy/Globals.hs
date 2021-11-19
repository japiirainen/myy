{-#LANGUAGE DataKinds #-}
-- Manages the global variables in Myy

module Myy.Globals where

import Myy.Type
import Myy.Exp

import Text.PrettyPrint.ANSI.Leijen

import Control.Monad.Error

import Data.Map as Map

-- | An existential wrapper around `Exp`, storing the expression and
--   it's type.
data EExp where
  EExp :: STy ty -> Exp '[] ty -> EExp
