{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Myy where

import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.List                  (foldl', isPrefixOf)
import qualified Data.Map                   as Map
import qualified Data.Text.Lazy             as L
import           Data.Text.Lazy.IO          as L
import qualified Myy.Env                    as Env
import           Myy.Eval
import           Myy.Infer
import           Myy.Parser
import           Myy.Pretty
import           Myy.Syntax

import           Prelude                    hiding (init)
import           System.Console.Repline
import           System.Environment
import           System.Exit

-- Types

data IState = IState
    { tyctx :: Env.Env
    , tmctx :: TermEnv
    }

initState :: IState
initState = IState Env.empty emptyTmEnv

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err)  = do
    liftIO $ print err
    abort

-- Execution

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
    where
        (_, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
    -- get the current interpreter state
    st <- get

    -- Parser ( returns AST )
    mod <- hoistErr $ parseModule "<stdin>" source

    -- Type Inference ( returns Typing Environment )
    tyctx' <- hoistErr $ inferTop (tyctx st) mod

    -- Create the new environment
    let st' = st { tmctx = foldl' evalDef (tmctx st) mod
                 , tyctx = tyctx' <> (tyctx st)
                 }

    -- Update the interpreter state
    when update (put st')

    case lookup "it" mod of
        Nothing -> return ()
        Just ex -> do
            let (val, _) = runEval (tmctx st') "it" ex
            showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
    case Env.lookup "it" (tyctx st) of
        Nothing  -> return ()
        Just val -> liftIO $ Prelude.putStrLn $ ppsignature (arg, val)

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-- Commands

-- :browse command
browse :: String -> Repl ()
browse _ = do
    st <- get
    liftIO $ mapM_ Prelude.putStrLn $ ppenv (tyctx st)

-- :load command
load :: String -> Repl ()
load arg = do
    let strippedArg = L.unpack $ L.strip (L.pack arg)
    contents <- liftIO $ L.readFile strippedArg
    exec True contents

-- :type command
typeof :: String -> Repl ()
typeof arg = do
    st <- get
    let strippedArg = L.unpack $ L.strip (L.pack arg)
    case Env.lookup strippedArg (tyctx st) of
        Nothing  -> exec False (L.pack arg)
        Just val -> liftIO $ Prelude.putStrLn $ ppsignature (arg, val)

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Interactive Shell

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
    ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
    let cmds = [":load", ":type", ":browse", ":quit"]
    Env.TypeEnv ctx <- gets tyctx
    let defs = Map.keys ctx
    return $ filter (isPrefixOf n) (cmds ++ defs)

ops :: [(String, String -> HaskelineT (StateT IState IO) ())]
ops = [
    ("load"   , dontCrash . load)
  , ("browse" , dontCrash .browse)
  , ("quit"   , dontCrash . quit)
  , ("type"   , dontCrash . Myy.typeof)
  ]

-- Entry Point

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher


shell :: Repl a -> IO ()
shell init = flip evalStateT initState
        $ evalReplOpts $ ReplOpts
        { banner           = const (pure "Myy> ")
        , command          = cmd
        , options          = Myy.ops
        , prefix           = Just ':'
        , multilineCommand = Nothing
        , tabComplete      = completer
        , initialiser      = void init
        , finaliser        = return Exit
        }

ini :: Repl ()
ini = liftIO $ Prelude.putStrLn "Welcome!"

-- Top-level

main :: IO ()
main = do
    args <- getArgs
    case args of
        []              -> shell (return ())
        [fname]         -> shell (load fname)
        ["test", fname] -> shell (load fname >> browse [] >> quit ())
        _               -> Prelude.putStrLn "invalid arguments"
