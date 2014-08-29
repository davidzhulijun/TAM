{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances,Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module PushdownAutomaton.State (
  PDAState(PDAState), 
  PDAStaten, initialStackn, initialPDAStaten,
  PDATransn, gcompile, translate
  ) where

import Basic.Types (GetValue, Forward, Trans(Trans), readMem, next)
import Basic.Memory --(Stack(..), empty, MapLike(..))
import Basic.Features (HasQ(..), HasMemory(..), HasIP(..))
import Control.Lens (makeLenses, (%~), (^.), (.~))
import PushdownAutomaton.Operations

---------------------------------------------------------------------
---- PDAState
---------------------------------------------------------------------

data PDAState qs stk ptr = PDAState {_cs :: qs, _mem :: stk, _inptp :: ptr}
makeLenses ''PDAState

instance HasQ (PDAState qs stk ptr) where
  type Q (PDAState qs stk ptr) = qs
  q = cs

instance HasMemory (PDAState qs stk ptr) where
  type Memory (PDAState qs stk ptr) = stk
  memory = mem

instance HasIP (PDAState qs stk ptr) where
  type IP (PDAState qs stk ptr) = ptr
  ip = inptp



---------------------------------------------------------------------
---- PDAState with n stack
---------------------------------------------------------------------
type PDAStaten qs map n stack s ptr = PDAState qs (map n (stack s)) ptr

initialStackn :: (Stack s, MapLike m) => v -> m a (s v) -> m a (s v)
initialStackn v mapn = mapWithKeyM mapn (\_ _ -> push v empty)

initialPDAStaten :: (Stack stack, MapLike map, Ord n, Bounded n, Enum n) => 
  qs -> v -> ptr -> PDAStaten qs map n stack v ptr
initialPDAStaten qs v ptr = PDAState qs (initialStackn v emptyM') ptr


type PDATransn m n ch s qs = ch -> m n s -> qs -> (qs, m n (StackCommd s))

gcompile :: (Ord a, HasQ st, HasIP st, HasMemory st, Stack s, MapLike m, 
  Memory st ~ m a (s v), Forward (IP st), GetValue (IP st) mem b ) =>
  (b -> m a v -> Q st -> (Q st, m a (StackCommd v))) -> Trans (x, mem b) st
gcompile f = Trans g
  where g (_, inpt) st = ip %~ next $ q .~ qs' $ memory %~ (stackoperate comds') $ st
          where  stkm = mapWithKeyM (st^.memory) (\_ -> top)
                 ch = readMem (st^.ip) inpt
                 (qs', comds') = f ch stkm (st^.q)

                 stackoperate comds stkmap = mapWithKeyM stkmap sop
                   where sop k sta = stackoperation (lookupM comds k) sta

translate :: forall a b qs m n. (Ord n, Bounded n, Enum n, AscListLike m) =>
  (a -> [b] -> qs -> (qs, [StackCommd b])) -> PDATransn m n a b qs
translate f inp stak qs = (qs', mapFromL (minBound::n) stakctrl)
  where 
    (qs', stakctrl) = f inp stak' qs
    stak' = map snd $ mapToL stak