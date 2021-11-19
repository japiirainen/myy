{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- The Myy Monad, allowing for pretty-printed output to the user, failing
-- with an error message, and tracking global variables

module Myy.Monad
  ( Myy
  , runMyy
  , prompt
  , quit
  , MyyE
  , runMyyE
  , issueError
  , eitherToMyyE
  , MyyM(..)
  ) where

import           Myy.Globals
import           Myy.Util

import           System.Console.Haskeline
import           Text.PrettyPrint.ANSI.Leijen

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           System.IO

-- | A monad giving Haskeline-like interaction, access to `Globals`,
--   and the ability to abort with `mzero`
newtype Myy a = Myy { unMyy :: MaybeT (StateT Globals (InputT IO)) a }
  deriving (Monad, Functor, Applicative, MonadState Globals, MonadIO)

-- | Like the `Myy` monad, but also supports error messages via `Doc`s
newtype MyyE a = MyyE { unMyyE :: ExceptT Doc Myy a }
  deriving (Monad, Functor, Applicative, MonadError Doc)

instance MonadReader Globals MyyE where
  ask = MyyE get
  local f thing_inside = MyyE $ do
    old_globals <- get
    put (f old_globals)
    result <- unMyyE thing_inside
    put old_globals
    return result

-- | Class for the two monads
class MyyM m where
  -- | Print a `Doc` without a newline at the end
  printDoc :: Doc -> m ()

  -- | Print a `Doc` with a newline
  printLine :: Doc -> m ()

instance MyyM Myy where
  printDoc = Myy . liftIO . displayIO stdout . toSimpleDoc
  printLine = Myy . liftIO . displayIO stdout . toSimpleDoc . (<> hardline)

instance MyyM MyyE where
  printDoc = MyyE . lift . printDoc
  printLine = MyyE . lift . printDoc

  -- | Prompt the user for input, returning a string if one is entered
  --   Like `getInputLine`
prompt :: String -> Myy (Maybe String)
prompt = Myy . lift . lift . getInputLine

-- | Abort the `Myy` monad
quit :: Myy a
quit = printLine (text "Good-bye.") *> Myy mzero

-- | Abort the computation with an error
issueError :: Doc -> MyyE a
issueError = MyyE . throwError

-- | Hoist an `Either` into `MyyE`
eitherToMyyE :: Either String a -> MyyE a
eitherToMyyE (Left e)  = issueError (text e)
eitherToMyyE (Right x) = return x

-- | Run a `Myy` computation
runMyy :: Myy () -> InputT IO ()
runMyy thing_inside
  = ignore $ flip evalStateT emptyGlobals $ runMaybeT $ unMyy thing_inside

-- | Run a `MyyE` computation
runMyyE :: MyyE a -> Myy (Either Doc a)
runMyyE = runExceptT . unMyyE
