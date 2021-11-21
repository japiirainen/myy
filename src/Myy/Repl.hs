{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- | Implements a REPL for myy

module Myy.Repl
    ( repl
    ) where

import           Prelude                      hiding (lex)

import           Myy.Check
import           Myy.Eval
import           Myy.Exp
import           Myy.Globals
import           Myy.Lex
import           Myy.Monad
import           Myy.Parse
import           Myy.Statement
import           Myy.Type
import           Myy.Unchecked
import           Myy.Util

import           Text.PrettyPrint.ANSI.Leijen as Pretty hiding ((<$>))

import           System.Console.Haskeline
import           System.Directory

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.List                    as List

repl :: IO ()
repl = runInputT defaultSettings $
        runMyy $ do
            helloWorld
            loop

loop :: Myy ()
loop = do
    m_line <- prompt "λ> "
    case stripWhitespace <$> m_line of
        Nothing          -> quit
        Just (':' : cmd) -> runCommand cmd
        Just str         -> runStmts str
    loop

-- | Prints welcome message
helloWorld :: Myy ()
helloWorld = do
    printLine lambda
    printLine $ text "Welcome to the Myy interpreter, version" <+>
                text version <> char '.'

lambda :: Doc
lambda = vcat $ List.map text
    [ "                   \\\\\\\\\\\\          "
    , "                    \\\\\\\\\\\\         "
    , "                     \\\\\\\\\\\\        "
    , "                      \\\\\\\\\\\\       "
    , "                       \\\\\\\\\\\\      "
    , "                      //\\\\\\\\\\\\     "
    , "                     ////\\\\\\\\\\\\    "
    , "                    //////\\\\\\\\\\\\   "
    , "                   //////  \\\\\\\\\\\\  "
    , "                  //////    \\\\\\\\\\\\ "
    ]

-- | The current version of myy
version :: String
version = "1.0"

-- running statements

runStmts :: String -> Myy ()
runStmts str = reportErrors $ do
    toks <- lexG str
    stmts <- parseStmtsG toks
    doStmts stmts

-- | Run a sequence of statements, returning the new global variables
doStmts :: [Statement] -> MyyE Globals
doStmts = foldr doStmt ask

doStmt :: Statement -> MyyE a -> MyyE a
doStmt (BareExp uexp) thing_inside = check uexp $ \sty exp -> do
    printLine $ printValWithType (eval exp) sty
    thing_inside
doStmt (NewGlobal g uexp) thing_inside = check uexp $ \sty exp -> do
    printLine $ text g <+> char '=' <+> printWithType exp sty
    local (extend g sty exp) thing_inside

-- commands

-- | Interpret a command (missing the initial ':')
runCommand :: String -> Myy ()
runCommand = dispatchCommand cmdTable

type CommandTable = [(String, String -> Myy ())]

dispatchCommand :: CommandTable -> String -> Myy ()
dispatchCommand table line =
    case List.filter ((cmd `List.isPrefixOf`) . fst) table of
        [] -> printLine $ text "Unknown command:" <+> squotes (text cmd)
        [(_, action)] -> action arg
        many -> do printLine $ text "Ambiguous command:" <+> squotes (text cmd)
                   printLine $ text "Possibilities:" $$
                               indent 2 (vcat $ List.map (text . fst) many)
    where (cmd, arg) = List.break isSpace line

cmdTable :: CommandTable
cmdTable = [ ("quit", quitCmd)
           , ("d-lex", lexCmd)
           , ("d-parse", parseCmd)
           , ("load", loadCmd)
           , ("eval", evalCmd)
           , ("step", stepCmd)
           , ("type", typeCmd)
           , ("all", allCmd)
           ]

quitCmd :: String -> Myy ()
quitCmd _ = quit

class Reportable a where
    report :: a -> Myy Globals

instance Reportable Doc where
    report x = printLine x >> get

instance Reportable () where
    report _ = get

instance Reportable Globals where
    report = return

instance {-# OVERLAPPABLE #-} Pretty a => Reportable a where
    report other = printLine (pretty other) >> get

reportErrors :: Reportable a => MyyE a -> Myy ()
reportErrors thing_inside = do
    result <- runMyyE thing_inside
    new_globals <- case result of
        Left err -> printLine err >> get
        Right x  -> report x
    put new_globals

parseLex :: String -> MyyE UExp
parseLex = parseExpG <=< lexG

printWithType :: (Pretty exp, Pretty ty) => exp -> ty -> Doc
printWithType exp ty
    = pretty exp <+> colon <+> pretty ty

printValWithType :: Val ty -> STy ty -> Doc
printValWithType val sty
    = prettyVal val sty <+> colon <+> pretty sty

lexCmd, parseCmd, evalCmd, stepCmd, typeCmd, allCmd, loadCmd
    :: String -> Myy ()
lexCmd expr = reportErrors $ lexG expr
parseCmd = reportErrors . parseLex

evalCmd expr = reportErrors $ do
    uexp <- parseLex expr
    check uexp $ \sty exp ->
        return $ printValWithType (eval exp) sty

stepCmd expr = reportErrors $ do
    uexp <- parseLex expr
    check uexp $ \sty exp -> do
        printLine $ printWithType exp sty
        let loop e = case step e of
                Left e' -> do
                    printLine $ text "-->" <+> printWithType e' sty
                    loop e'
                Right v -> return v
        v <- loop exp
        return $ printValWithType v sty

typeCmd expr = reportErrors $ do
    uexp <- parseLex expr
    check uexp $ \sty exp ->
        return $ printWithType exp sty

allCmd expr = do
    printLine (text "Small step:")
    _ <- stepCmd expr
    printLine Pretty.empty
    printLine (text "Big step:")
    evalCmd expr

loadCmd (stripWhitespace -> file) = do
    file_exists <- liftIO $ doesFileExist file
    if not file_exists then file_not_found else do
        contents <- liftIO $ readFile file
        runStmts contents
    where
        file_not_found = do
            printLine (text "File not found:" <+> squotes (text file))
            cwd <- liftIO getCurrentDirectory
            printLine (parens (text "Current directory:" <+> text cwd))
