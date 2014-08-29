{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module PushdownAutomaton.Models.PDA3 (
  mach, mach2
  )  where

import Basic.Types
import Basic.Memory
import Basic.MemoryImpl --(StackMem, ListMem, fillMem, MapMem, mapToList, mapFromList, Address(..))
import PushdownAutomaton.Operations
import PushdownAutomaton.State
import PushdownAutomaton.Machine
import qualified PushdownAutomaton.Machine2 as M2
import qualified PushdownAutomaton.State2 as S2
--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
---------------------------------------------------------------------------
-- DPDA - II (2 push-down store)
-- example from pdf on the expressive power of 2 stack visibly pushdown automata
-- http://www.lsv.ens-cachan.fr/~bollig/exposes/mvpa.pdf
-- Type 2: Pushdown Automaton
data CState = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 deriving (Show, Eq)
data InpAlp = Zero | One | Empty deriving (Show, Eq)
type StackAlp = InpAlp

trans :: [InpAlp] -> [StackAlp] -> CState -> (CState, [StackCommd StackAlp])
trans [Zero, Empty, Empty] [Empty, Empty] Q0 = (Q2, [Push Zero, None])
trans [Zero, Empty, Empty] [_, _] Q1 = (Q2, [Push Zero, None])
trans [Empty, Zero, Empty] [_, Empty] Q2 = (Q1, [None, Push Zero])
trans [Empty, Zero, Empty] [_, _] Q2 = (Q3, [None, Push Zero])
trans [One , Empty, Empty] [Empty, _] Q3 = (Q4, [None, None])
trans [One , Empty, Empty] [_, _] Q3 = (Q3, [Pop, None])
trans [Empty,  One, Empty] [_, Empty] Q4 = (Q5, [None, None])
trans [Empty,  One, Empty] [_, _] Q4 = (Q4, [None, Pop])   
trans a    b   c = error ("undefined tr: " ++ show a ++ ", "
      ++ show b ++ ", " ++ show c)

input :: ListMem [InpAlp]
input = fillMem [[Zero, Empty, Empty],[Empty, Zero, Empty],[Zero, Empty, Empty],[Empty, Zero, Empty],[One , Empty, Empty],[One , Empty, Empty],[One , Empty, Empty],[Empty,  One, Empty],[Empty,  One, Empty],[Empty,  One, Empty]]
finalState   :: ListMem CState
finalState   = fillMem [Q5]

instance Pointed StackAlp where
    pt = Empty

initialState :: (Pointed StackAlp) => PDAStaten CState MapMem ITwo StackMem StackAlp (Address Int)
initialState = initialPDAStaten Q0 Empty (A (0::Int))


mach :: PDAn CState MapMem ITwo StackMem StackAlp (Address Int) ListMem ListMem [InpAlp]
mach = PDA initialState finalState (gcompile $ translate trans) input

initialState2 :: CState
initialState2 = Q0

initialMem2 :: MapMem ITwo (StackMem StackAlp)
initialMem2 = S2.initialStackn Empty emptyM'

mach2 :: M2.PDAn CState MapMem ITwo StackMem StackAlp ListMem ListMem [InpAlp]
mach2 = M2.PDA initialState2 initialMem2 finalState (S2.gcompile $ S2.translate trans) input