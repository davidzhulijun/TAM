{-# LANGUAGE MultiParamTypeClasses, TypeFamilies#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module LambdaCalculus.State (
  SECDState(SECDState), emptySECD
  ) where

import Basic.Memory (Stack, MapLike, empty, emptyM)
import LambdaCalculus.Types -- pretty much everything from this is needed
import Control.Lens (makeLenses)


--------------------------------------------------------------------
-- one example

data SECDState s m st c = SECDState
  { _stk :: st s, _envi :: m, _control :: st c, 
    _dmp :: st (st s, m, st c)}
makeLenses ''SECDState

instance HasStack (SECDState s m st c) where
  type Stck (SECDState s m st c) = st s
  stck = stk

instance HasEnv (SECDState s m st c) where
  type Env (SECDState s m st c) = m
  env = envi

instance HasCtrl (SECDState s m st c) where
  type Ctrl (SECDState s m st c) = st c
  ctrl = control

instance HasDump (SECDState s m st c)  where
  type Dump (SECDState s m st c) = st (st s, m, st c)
  dump = dmp

emptySECD :: (Stack st, MapLike m) => SECDState s (m var val) st c
emptySECD = SECDState empty emptyM empty empty