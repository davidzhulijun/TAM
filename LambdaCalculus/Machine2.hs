{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module LambdaCalculus.Machine2 (
    SECD(SECD), SECD1
  ) where

import Basic.Types
import Basic.Memory (isEmpty, Stack)
import Basic.Features
import LambdaCalculus.Types

import Control.Lens

--------------------------------------------------------------------------
---------------------------- SECD Machine
--------------------------------------------------------------------------

data SECD s e c d =
  SECD { _stk :: s, 
         _envi :: e, 
         _control :: c, 
         _dmp ::  d,
         _tran :: Trans () (s, e, c, d)}
makeLenses ''SECD

instance HasStack (SECD s e c d) where
  type Stck (SECD s e c d) = s
  stck = stk

instance HasEnv (SECD s e c d) where
  type Env (SECD s e c d) = e
  env = envi

instance HasCtrl (SECD s e c d) where
  type Ctrl (SECD s e c d) = c
  ctrl = control

instance HasDump (SECD s e c d)  where
  type Dump (SECD s e c d) = d
  dump = dmp

instance HasTransition (SECD s e c d) where
  transition = tran

instance  HasState (SECD s e c d) where
  type State (SECD s e c d) = (s, e, c, d)
  state g (SECD s e c d tr) = fmap (\(x1,x2,x3,x4) -> SECD x1 x2 x3 x4 tr) (g (s, e, c, d))

instance HasStatic (SECD s e c d) where
  type Static (SECD s e c d) = ()
  static g (SECD s e c d tr)= fmap (\() -> SECD s e c d tr) (g ())

type SECD1 st s e c = SECD (st s) e (st c) (st (st s, e, st c))

instance (Stack st) => EndCond (SECD1 st s e c) where
  endcond mach = isEmpty (mach^.ctrl) && isEmpty (mach^.dump)
