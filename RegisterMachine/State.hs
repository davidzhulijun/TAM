{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.State (
    CounterMachineState(..), emptyCM, initCM,
    CMAState(..), initedCMA,
    compile
    ) where

import Basic.Types
import Basic.Memory (Empty(..), Initialized(..))
import Basic.Features

import Control.Lens

--------------------------------------------------------------------------
---------------------- Level 3 Instance of Register Machines with state
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- 3.1 state with a  memory and a single instruction register pointer
--------------------------------------------------------------------------
data CounterMachineState v mem ptr = CM {_dat :: mem v, _addr :: ptr}
makeLenses ''CounterMachineState

instance HasQ (CounterMachineState v mem ptr) where
  type Q (CounterMachineState v mem ptr) = ptr
  q = addr

instance HasHeap (CounterMachineState v mem ptr) where
  type Heap (CounterMachineState v mem ptr) = mem
  type HContents (CounterMachineState v mem ptr) = v
  heap = dat

-- 3.1.1 useful initial state
emptyCM :: (Pointed i, Empty m, Initialized a) => CounterMachineState v m (a i)
emptyCM = CM empty inited

initCM :: (Pointed v, Pointed i, Initialized m, Initialized a) => CounterMachineState v m (a i)
initCM = CM inited inited

--------------------------------------------------------------------------
-- 3.2 state with a heap, a single ir pointer and an accumulator
--------------------------------------------------------------------------
data CMAState dat acc mem ptr = 
  CMA { _accum   :: acc dat
      , _accdat  :: mem dat
      , _accaddr :: ptr}
makeLenses ''CMAState

instance HasQ (CMAState v acc mem ptr) where
  type Q (CMAState v acc mem ptr) = ptr
  q = accaddr

instance HasHeap (CMAState v acc mem ptr) where
  type Heap (CMAState v acc mem ptr) = mem
  type HContents (CMAState v acc mem ptr) = v
  heap = accdat

instance HasACC (CMAState v acc mem ptr) where
  type ACC (CMAState v acc mem ptr) = acc
  type AContents (CMAState v acc mem ptr) = v
  acc = accum

-- 3.2.1 useful initial state
initedCMA :: (Pointed v, Pointed i, Initialized m, Initialized addr, Initialized acc) => CMAState v acc m (addr i)
initedCMA = CMA inited inited inited

--------------------------------------------------------------------------
-- 3.3 state with a single data register and a single ir pointer
--------------------------------------------------------------------------
-- Not used?
-- type SingleCounterMachineState v ptr = CounterMachineState v SingleLoc ptr

--------------------------------------------------------------------------
-- 3.4 state with single data register and a single ir pointer and a accumerator
--------------------------------------------------------------------------
-- Not used?
-- type SCMAState v ptr = CMAState v SingleLoc SingleLoc ptr

--------------------------------------------------------------------------

compile :: (GetValue (Q st) d a, Forward (Q st), HasQ st) =>
  (a -> st -> st) -> Trans (d a) st
compile f = Trans g
    where g (inpt) qs = qs'
            where ch = readMem (qs^.q) inpt
                  qs' = f ch qs
