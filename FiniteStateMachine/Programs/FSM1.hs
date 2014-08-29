{-# OPTIONS -Wall #-}

module FiniteStateMachine.Programs.FSM1 (
  mach
  ) where

import Basic.MemoryImpl (ListMem, fillMem, Address(..))
import FiniteStateMachine.State
import FiniteStateMachine.Machine

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
data CState = Lock | Open deriving (Show, Eq)
data InpAlp  = Coin | Push deriving Show

trans :: FSMTrans InpAlp CState
trans Coin  Lock = Open
trans Push  Lock = Lock
trans Coin  Open = Open
trans Push  Open = Lock

initialState :: FSMState CState (Address Int)
initialState = initialFSMState Open (A (0::Int))
finalState :: ListMem CState
finalState = fillMem [Lock]
input :: ListMem InpAlp
input = fillMem [Coin, Coin, Coin, Push]

mach :: FSM1 CState (Address Int) InpAlp ListMem
mach = FSM initialState finalState (compile trans) input