{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances,Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module PushdownAutomaton.State2 (
  PDAState(PDAState), 
  PDAStaten, initialStackn, initialPDAStaten,
  PDATransn, gcompile, translate
  ) where

import Basic.Types (Forward, Trans(Trans), next, Copointed(copoint))
import Basic.Memory --(Stack(..), empty, MapLike(..))
import Basic.Features (HasQ(..), HasMemory(..), HasInput(..))
import Control.Lens (makeLenses, (^.))
import PushdownAutomaton.Operations

---------------------------------------------------------------------
---- PDAState
---------------------------------------------------------------------

data PDAState qs stk inptt = PDAState {_cs :: qs, _mem :: stk, _inpt :: inptt}
makeLenses ''PDAState

instance HasQ (PDAState qs stk inptt) where
  type Q (PDAState qs stk inptt) = qs
  q = cs

instance HasMemory (PDAState qs stk inptt) where
  type Memory (PDAState qs stk inptt) = stk
  memory = mem

instance HasInput (PDAState qs stk inptt) where
  type Input (PDAState qs stk inptt) = inptt
  input = inpt

---------------------------------------------------------------------
---- PDAState with n stack
---------------------------------------------------------------------
type PDAStaten qs map n stack s inptt = PDAState qs (map n (stack s)) inptt

initialStackn :: (Stack s, MapLike m) => v -> m a (s v) -> m a (s v)
initialStackn v mapn = mapWithKeyM mapn (\_ _ -> push v empty)

initialPDAStaten :: (Stack stack, MapLike map, Ord n, Bounded n, Enum n) => 
  qs -> v -> inptt -> PDAStaten qs map n stack v inptt
initialPDAStaten qs v inptt = PDAState qs (initialStackn v emptyM') inptt

type PDATransn m n ch s qs = ch -> m n s -> qs -> (qs, m n (StackCommd s))

translate :: forall a b qs m n. (Ord n, Bounded n, Enum n, AscListLike m) =>
  (a -> [b] -> qs -> (qs, [StackCommd b])) -> PDATransn m n a b qs
translate f inp stak qs = (qs', mapFromL (minBound::n) stakctrl)
  where 
    (qs', stakctrl) = f inp stak' qs
    stak' = map snd $ mapToL stak

gcompile :: (Forward (mem i),Copointed mem,  Ord n, MapLike m, Stack stack ) =>
  (i -> m n v -> qs -> (qs, m n (StackCommd v))) -> Trans x (PDAState qs (m n (stack v)) (mem i))
gcompile f = Trans g
  where g _ st = PDAState qs' (stackoperate comds' (st^.memory)) (next (st^.input))
          where  stkm = mapWithKeyM (st^.memory) (\_ -> top)
                 ch = copoint (st^.input)
                 (qs', comds') = f ch stkm (st^.q)

                 stackoperate comds stkmap = mapWithKeyM stkmap sop
                   where sop k sta = stackoperation (lookupM comds k) sta