{-# LANGUAGE LambdaCase #-}
module Myy.Eval where

import           Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map               as Map
import           Myy.Syntax

data Value
    = VInt Integer
    | VBool Bool
    | VClosure String Expr TermEnv

type TermEnv = Map.Map String Value

type Interpreter a = Identity a

emptyTmEnv :: TermEnv
emptyTmEnv = Map.empty

instance Show Value where
    show (VInt i)   = show i
    show (VBool b)  = show b
    show VClosure{} = "<<closure>>"

eval :: TermEnv -> Expr -> Interpreter Value
eval env = \case
    Lit (LInt i)  -> return $ VInt i
    Lit (LBool b) -> return $ VBool b

    Var x -> do
        let Just v = Map.lookup x env
        return v

    Op op a b -> do
        a' <- eval env a
        b' <- eval env b
        case (a', b') of
            (VInt ai, VInt bi) -> return $ (binop op) ai bi
            _                  -> error "impossible"

    Lam x body ->
         return $ VClosure x body env

    App fun arg -> do
        eval env fun >>= \case
            VClosure x body clo -> do
                argv <- eval env arg
                let nenv = Map.insert x argv clo
                eval nenv body
            _ -> error "impossible"

    Let x e body -> do
        e' <- eval env e
        let nenv = Map.insert x e' env
        eval nenv body

    If cond tr fl -> do
        eval env cond >>= \case
            VBool b -> eval env $ if b then tr else fl
            _       -> error "impossible"

    Fix e -> do
        eval env (App e (Fix e))

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt (a + b)
binop Sub a b = VInt (a - b)
binop Mul a b = VInt (a * b)
binop Eql a b = VBool (a == b)

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
    let res = runIdentity (eval env ex)
    in (res, Map.insert nm res env)
