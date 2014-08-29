{-# LANGUAGE MultiParamTypeClasses, TypeFamilies#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module LambdaCalculus.State2 (
  SECDState(SECDState), emptySECD, SECDState1
  ) where

import Basic.Memory (Stack, MapLike, empty, emptyM)
import LambdaCalculus.Types -- pretty much everything from this is needed
import Control.Lens (makeLenses)


--------------------------------------------------------------------
-- one example

data SECDState s e c d = SECDState
  { _stk :: s, _envi :: e, _control :: c, 
    _dmp :: d}
makeLenses ''SECDState

instance HasStack (SECDState s e c d) where
  type Stck (SECDState s e c d) = s
  stck = stk

instance HasEnv (SECDState s e c d) where
  type Env (SECDState s e c d) = e
  env = envi

instance HasCtrl (SECDState s e c d) where
  type Ctrl (SECDState s e c d) = c
  ctrl = control

instance HasDump (SECDState s e c d)  where
  type Dump (SECDState s e c d) = d
  dump = dmp

type SECDState1 s m st c = SECDState (st s) m (st c) (st (st s, m, st c))

emptySECD :: (Stack st, MapLike m) => SECDState1 s (m var val) st c
emptySECD = SECDState empty emptyM empty empty