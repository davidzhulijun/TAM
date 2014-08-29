{-# LANGUAGE TypeFamilies#-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Programs.FSM2 (
  mach
  ) where

import Basic.MemoryImpl (ListMem, fillMem)
import FiniteStateMachine.Machine2

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
data CState = Lock | Open deriving (Show, Eq)
data InpAlp  = Coin | Push deriving Show

trans :: InpAlp -> CState -> CState
trans Coin  Lock = Open
trans Push  Lock = Lock
trans Coin  Open = Open
trans Push  Open = Lock

initialState :: CState
initialState = Open
finalState :: ListMem CState
finalState = fillMem [Lock]
input :: ListMem InpAlp
input = fillMem [Coin, Coin, Coin, Push]

mach :: FSM1 CState InpAlp ListMem
mach = FSM initialState finalState (compile trans) input
