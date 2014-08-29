{-# OPTIONS -Wall #-}

module FiniteStateMachine.Programs.FSM4 (
  mach
  ) where

import Basic.MemoryImpl (ListMem, fillMem)
import FiniteStateMachine.Machine2

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
data CState = S1 | S2 | S3 | S4 | S5 deriving (Show, Eq)
data InpAlp  = A0 | A1 deriving Show

trans :: InpAlp -> CState -> CState
trans A0  S1 = S3
trans A1  S1 = S5
trans A0  S2 = S3
trans A1  S2 = S5
trans A0  S3 = S2
trans A1  S3 = S1
trans A0  S4 = S4
trans A1  S4 = S5
trans A0  S5 = S4
trans A1  S5 = S1

initialState :: CState
initialState = S1
finalState :: ListMem CState
finalState = fillMem []
input :: ListMem InpAlp
input = fillMem [A0, A1, A0, A1, A0, A1, A1, A0, A1, A0, A0]

mach :: FSM1 CState InpAlp ListMem
mach = FSM initialState finalState (compile trans) input