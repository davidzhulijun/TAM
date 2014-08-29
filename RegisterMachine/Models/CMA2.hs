{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA2 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.2. in Minsky (1967)
-----------------------------------------
data Lang i = INCA | CPA i i | JERA i i | ONRA i i i | HALTA
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALTA = True
  isHaltInstr _    = False
  
trans :: (Zero (HContents st), RWValue IMM (ACC st) (AContents st),
  One (HContents st), Incr v, Incr (AContents st),
  RWValue (Address v) (Heap st) (HContents st), HasQ st, HasHeap st, HasACC st,
  Q st ~ Address v) => Lang v -> st -> st
trans (INCA)       = incrACC
trans (CPA r1 r2)  = copyMem (A r1) (A r2)
trans (JERA r1 a1) = jumpe0Mem (A r1) (A a1)
trans (ONRA r1 r2 d) = stateful_If (cmpe2 heap (A r1) heap (A r2)) (clearMem (A d)) (ldaMem (A d) one)
trans HALTA        = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CPA 0 1, INCA, HALTA]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
