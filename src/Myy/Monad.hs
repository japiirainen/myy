-- The Myy Monad, allowing for pretty-printed output to the user, failing
-- with an error message, and tracking global variables

module Myy.Monad where

import           Myy.Globals
import           Myy.Util

import           System.Console.Haskeline
import           Text.PrettyPrint.ANSI.Leijen

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           System.IO

