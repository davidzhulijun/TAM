{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module PushdownAutomaton.Models.PDA1 (
  mach, mach2
  ) where

import Basic.Types
import Basic.Memory
import Basic.MemoryImpl --(StackMem, ListMem, fillMem, Address(..))
import PushdownAutomaton.Operations
import PushdownAutomaton.State
import PushdownAutomaton.Machine
import qualified PushdownAutomaton.Machine2 as M2
import qualified PushdownAutomaton.State2 as S2
--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DPDA-I (1 push-down store)
-- an exampel from Example 5.1 of Book Introduction to automata theory, languages, and computation
-- Type 2: Pushdown Automaton
data CState = P | Q | R deriving (Show, Eq)
data InpAlp = Zero | One deriving (Show, Eq)
data StackAlp = B | Z deriving (Show, Eq)

trans :: [InpAlp] -> [StackAlp] -> CState -> (CState, [StackCommd StackAlp])
trans [Zero] [Z] P = (P, [Push B])
trans [Zero] [B] P = (P, [Push B])
trans [One]  [Z] P = (Q, [None])
trans [One]  [B] P = (Q, [None])
trans [One]  [B] Q = (Q, [Pop])
trans _    [Z] Q = (R, [None])
trans a    b   c = error ("undefined tr: " ++ show a ++ ", "
      ++ show b ++ ", " ++ show c)

finalState   :: ListMem CState
finalState   = fillMem [R]
input :: ListMem [InpAlp]
input = fillMem [[Zero], [Zero], [One], [One], [One], [One]]

instance Pointed StackAlp where
    pt = B

initialState :: (Pointed StackAlp) => PDAStaten CState MapMem IOne StackMem StackAlp (Address Int)
initialState = initialPDAStaten P B (A (0::Int))

mach :: PDAn CState MapMem IOne StackMem StackAlp (Address Int) ListMem ListMem [InpAlp]
mach = PDA initialState finalState (gcompile $ translate trans) input

initialState2 :: CState
initialState2 = P
initialMem2 :: MapMem IOne (StackMem StackAlp)
initialMem2 = S2.initialStackn B emptyM'

mach2 :: M2.PDAn CState MapMem IOne StackMem StackAlp ListMem ListMem [InpAlp]
mach2 = M2.PDA initialState2 initialMem2 finalState (S2.gcompile $ S2.translate trans) input