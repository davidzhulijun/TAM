{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA4 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.4. 1958: Peter's treatment
-----------------------------------------
data Lang i = CLR i | CPA i i | CPAI i i | JER i i i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st), 
  Decr v, HasQ st, HasHeap st, Q st ~ Address v) =>
  Lang v -> st -> st
trans (CLR r) = clearMem (A r)
trans (CPA r1 r2)  = copyMem (A r1) (A r2)
trans (CPAI r1 r2)  = previousQ . incrMem (A r2) . copyMem (A r1) (A r2) 
trans (JER r1 r2 a1 a2) =  jumpe2 heap (A r1) heap (A r2) (A a1) (A a2)
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CLR 0, CPA 0 1 , CPAI 0 1, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
