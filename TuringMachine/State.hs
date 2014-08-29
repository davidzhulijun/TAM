{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}


module TuringMachine.State (
    TMState(TMState), 
   -- TMState1, initialTMState, TMTrans, compile,
    --TMState2, initialTMState2, TMTrans2, compile2,
    TMStaten, initialTMStaten, TMTransn,  gcompile, translate
	) where

import Basic.Types
import Basic.Memory
import Basic.Features
import Control.Lens

---------------------------------------------------------------------
---- Turing Machine State
---------------------------------------------------------------------
----------------------------------------------------------------------
-- 5.1 State of Turing Machine
-- q - type of states
-- t - type of characters on tape
-- tape - container type of type (should be of class FixedTape/MoveTape)
data TMState tap qs = TMState {_tape :: tap, _ir :: qs}
makeLenses ''TMState

{- This is not actually used anywhere!  This implies a lack of generality.-}
instance HasMemory (TMState tap qs) where
  type Memory (TMState tap qs) = tap
  memory = tape


instance HasQ (TMState tap qs) where
  type Q (TMState tap qs) = qs
  q = ir

----------------------------------------------------------------------
-- Generic Compile Function (with n tapes and n threads)
----------------------------------------------------------------------

type TMStaten n map t tape qs = TMState (map n (tape t)) qs

initialTapen :: (Ord a, Pointed v, FixedTape s, MapLike map) =>
    v -> map a (s v) -> map a (s v)
initialTapen v mapn = mapWithKeyM mapn (\_ _ -> tapewrite inited v)

initialTMStaten :: (FixedTape tape, Pointed t1, Ord i, Enum i, 
  Bounded i, InitMapLike map) => t1 -> qs -> TMStaten i map t1 tape qs
initialTMStaten v1 st = TMState (initialTapen v1 initM) st

-- Convert between the more traditional representation of a TM and
-- a more 'universal' representation:
type TMTransn m n a b d = a -> m n b -> (a, m n b, m n d)

gcompile :: (Ord a, HasQ st, HasMemory st, MoveTape s d,
  MapLike m1, MapLike m2, MapLike m3, Memory st ~ m2 a (s v)) =>
    (Q st -> m2 a v -> (Q st, m1 a v, m3 a d)) -> Trans x st
gcompile f = Trans g
  where g _ st = q .~ qs' $ memory %~ (tapeoperate v' d') $ st
          where v = mapWithKeyM (st^.memory) (\_ -> taperead)
                (qs', v', d') = f (st^.q) v

                tapeoperate vals moves tapmap = mapWithKeyM tapmap sop
                  where sop k tp = tapemove d $ tapewrite tp $ lookupM vals k
                          where d = lookupM moves k

translate :: forall a b d m n. (Ord n, Bounded n, Enum n, AscListLike m) =>
    (a -> [b] -> (a, [b], [d])) -> TMTransn m n a b d
translate f q0 vals = (q', mapFromL (minBound::n) vals', mapFromL (minBound::n) dirs)
  where
    (q', vals', dirs) = f q0 v
    v = map snd $ mapToL vals
