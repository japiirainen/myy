module Myy.Type where

newtype TVar = TV String
    deriving (Eq, Ord, Show)

data Type
    = TVar TVar
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"
