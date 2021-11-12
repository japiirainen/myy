module Myy.Env
    ( Env(..)
    , empty
    , lookup
    , remove
    , extend
    , extends
    , merge
    , mergeEnvs
    , singleton
    , keys
    , fromList
    , toList
    ) where

import           Data.Foldable hiding (toList)
import           Data.Monoid
import           Myy.Syntax
import           Myy.Type
import           Prelude       hiding (lookup)

import qualified Data.Map      as Map

-- Typing Environment

data Env = TypeEnv { types :: Map.Map Name Scheme }
    deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv env) = Map.lookup key env

merge :: Env -> Env -> Env
merge (TypeEnv env1) (TypeEnv env2) = TypeEnv (Map.union env1 env2)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv $ Map.singleton x y

keys :: Env -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> Env
fromList = TypeEnv . Map.fromList

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Semigroup Env where
    e1 <> e2 = e1 `merge` e2

instance Monoid Env where
    mempty = empty
