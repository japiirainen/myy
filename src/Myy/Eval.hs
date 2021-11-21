{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
-- | Myy expression evaluator for checked expressions.

module Myy.Eval
    ( eval
    , step
    ) where

import           Myy.Exp
import           Myy.Shift
import           Myy.Token

-- | Given a lambda and an expression, beta-reduce.
apply :: Val (arg -> res) -> Exp '[] arg -> Exp '[] res
apply (LamVal body) arg = subst arg body

-- | Apply an arithmetic operator to two values.
arith :: Val Int -> ArithOp ty -> Val Int -> Exp '[] ty
arith (IntVal n1) Plus (IntVal n2)     = IntE (n1 + n2)
arith (IntVal n1) Minus (IntVal n2)    = IntE (n1 - n2)
arith (IntVal n1) Times (IntVal n2)    = IntE (n1 * n2)
arith (IntVal n1) Divide (IntVal n2)   = IntE (n1 `div` n2)
arith (IntVal n1) Mod (IntVal n2)      = IntE (n1 `mod` n2)
arith (IntVal n1) Less (IntVal n2)     = BoolE (n1 < n2)
arith (IntVal n1) LessE (IntVal n2)    = BoolE (n1 <= n2)
arith (IntVal n1) Greater (IntVal n2)  = BoolE (n1 > n2)
arith (IntVal n1) GreaterE (IntVal n2) = BoolE (n1 >= n2)
arith (IntVal n1) Equals (IntVal n2)   = BoolE (n1 == n2)

-- | Conditionally choose between two expressions.
cond :: Val Bool -> Exp '[] t -> Exp '[] t -> Exp '[] t
cond (BoolVal True) e _  = e
cond (BoolVal False) _ e = e

-- | Unroll a `fix` one level
unfix :: Val (ty -> ty) -> Exp '[] ty
unfix (LamVal body) = subst (Fix (Lam body)) body

-- | A well-typed variable in an empty context is impossible
impossibleVar :: Elem '[] x -> a
impossibleVar _ = error "GHC's typechecker failed"

-- | Evaluate an expression, using big-step semantics.
eval :: Exp '[] t -> Val t
eval (Var v)          = impossibleVar v
eval (Lam body)       = LamVal body
eval (App e1 e2)      = eval (apply (eval e1) e2)
eval (Arith e1 op e2) = eval (arith (eval e1) op (eval e2))
eval (Cond e1 e2 e3)  = eval (cond (eval e1) e2 e3)
eval (Fix e)          = eval (unfix (eval e))
eval (IntE n)         = IntVal n
eval (BoolE b)        = BoolVal b

-- | Step an expression, either to another expression or to a value.
step :: Exp '[] t -> Either (Exp '[] t) (Val t)
step (Var v) = impossibleVar v
step (Lam body) = Right (LamVal body)
step (App e1 e2) = case step e1 of
    Left e1'            -> Left (App e1' e2)
    Right (LamVal body) -> Left (subst e2 body)
step (Arith e1 op e2) = case step e1 of
    Left e1' -> Left (Arith e1' op e2)
    Right v1 -> case step e2 of
        Left e2' -> Left (Arith (val v1) op e2')
        Right v2 -> Left (arith v1 op v2)
step (Cond e1 e2 e3) = case step e1 of
    Left e1' -> Left (Cond e1' e2 e3)
    Right v1 -> Left (cond v1 e2 e3)
step (Fix e) = case step e of
    Left e' -> Left (Fix e')
    Right v -> Left (unfix v)
step (IntE n) = Right (IntVal n)
step (BoolE b) = Right (BoolVal b)
