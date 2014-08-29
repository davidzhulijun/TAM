{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module LambdaCalculus.Types (
  Var(..), Const(..), Arity, Prim(..), Term(..), Atom(..),
  HasStack(..), HasEnv(..), HasCtrl(..), HasDump(..), HasSECDState,
  primArity
  ) where

import Control.Lens (Simple, Lens)
-- Lambda Calculus Features
-- Lambda Calculus Types
newtype Var = Var String
              deriving (Eq, Ord, Show, Read)

data Const = IntegerC Int
           | UnitC
           | CharC Char
           deriving (Eq, Ord, Show, Read)

type Arity = Int

data Prim = UnitPP
          | IntegerPP
          | LambdaPP
          | ApplicationPP
          | FunctionPP
          deriving (Eq, Ord, Show, Read, Enum, Bounded)

primArity :: Prim -> Arity
primArity UnitPP = 1
primArity IntegerPP = 1
primArity LambdaPP = 1
primArity ApplicationPP = 1
primArity FunctionPP = 0

data Term = VarT Var
          | ConstT Const
          | PrimT  Prim
          | AbsT Var Term
          | AppT Term Term
            deriving (Eq, Show, Read)

data Atom m = ErrorA String
            | ConstA Const
            | PrimA Prim [Atom m]
            | ClosureA Var Term (m Var (Atom m))

deriving instance Show (m Var (Atom m)) => Show (Atom m)

-- Generic structure (should be moved out of there)
class HasStack x where
  type Stck x :: *
  stck :: Simple Lens x (Stck x)

class HasEnv x where
  type Env x :: *
  env :: Simple Lens x (Env x)

class HasCtrl x where
  type Ctrl x :: *
  ctrl :: Simple Lens x (Ctrl x)

class HasDump x where
  type Dump x :: *
  dump :: Simple Lens x (Dump x)

class (HasStack x, HasEnv x, HasCtrl x, HasDump x) => HasSECDState x where
