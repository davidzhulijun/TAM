{-# OPTIONS -Wall #-}

module FiniteStateMachine.Programs.FSM5 (
  mach, mach2
  ) where

import Basic.MemoryImpl (ListMem, fillMem, Address(..))
import qualified FiniteStateMachine.Machine4 as M1
import qualified FiniteStateMachine.Machine5 as M2

--------------------------------------------------------------------------
--------------------------specilized model operations & models
-- Mealy Machine 
--http://www.iro.umontreal.ca/~dift6221/demicheli4/fsm.4.ps.pdf
--------------------------------------------------------------------------
data CState = S1 | S2 | S3 | S4 | S5 deriving (Show, Eq)
data InpAlp  = A0 | A1 deriving Show
data OutpAlp = B0 | B1 deriving Show 

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

transout :: InpAlp -> CState -> OutpAlp
transout A0  S1 = B1
transout A1  S1 = B1
transout A0  S2 = B1
transout A1  S2 = B1
transout A0  S3 = B0
transout A1  S3 = B1
transout A0  S4 = B0
transout A1  S4 = B1
transout A0  S5 = B1
transout A1  S5 = B0

initialState :: CState
initialState = S1
initialState2 :: M2.FSMState CState (Address Int)
initialState2 = M2.initialFSMState S1 (A (0::Int))

finalState :: ListMem CState
finalState = fillMem []
input :: ListMem InpAlp
input = fillMem [A0, A1, A0, A1, A0, A1, A1, A0, A1, A0, A0]
output :: ListMem OutpAlp
output = fillMem []

mach :: M1.FSM1 CState InpAlp ListMem OutpAlp
mach = M1.FSM initialState finalState (M1.compile trans) input output (M1.outputcompile transout)

mach2 :: M2.FSM1 CState InpAlp ListMem OutpAlp (Address Int)
mach2 = M2.FSM initialState2 finalState (M2.compile trans) input output (M2.outputcompile transout)