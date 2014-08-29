{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA5 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.5. 1961: Minsky's model of a partial recursive function reduced to a program of only two instructions
-----------------------------------------
data Lang i = ADDJM i i | SUBJM i i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Ord (HContents c), Zero (HContents c),
  RWValue (Address v) (Heap c) (HContents c), Incr v, Incr (HContents c),
  Decr (HContents c), HasQ c,
  HasHeap c, Q c ~ Address v) =>
  Lang v -> c -> c
trans (ADDJM r a) = jjump (A a) . incrMem (A r) 
trans (SUBJM r a1 a2)  = stateful_If (cmp0 (>=) heap (A r)) (jjump (A a2)) (jjump (A a1) . decrMem (A r))
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [ADDJM 1 1, SUBJM 1 1 2, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
