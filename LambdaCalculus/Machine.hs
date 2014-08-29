{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module LambdaCalculus.Machine (
    SECD(SECD)
  ) where

import Basic.Types
import Basic.Memory (isEmpty, Stack)
import Basic.Features
import LambdaCalculus.Types
import LambdaCalculus.State

import Control.Lens

--------------------------------------------------------------------------
---------------------------- SECD Machine
--------------------------------------------------------------------------

data SECD st =
  SECD { _stat :: st
       , _tran :: Trans () st}
makeLenses ''SECD

instance  HasState (SECD st) where
  type State (SECD st) = st
  state = stat

instance HasTransition (SECD st) where
  transition = to $ view tran

instance HasStatic (SECD st) where
  type Static (SECD st) = ()
  static g (SECD st tr)= fmap (\() -> SECD st tr) (g ())

instance (Stack st) => EndCond (SECD (SECDState s m st c)) where
  endcond mach = isEmpty (st^.ctrl) && isEmpty (st^.dump) where
    st = mach^.state
