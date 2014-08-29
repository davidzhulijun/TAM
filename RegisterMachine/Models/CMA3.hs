{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA3 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.3. 1958: Ershov's class of operator algorithms
-----------------------------------------
data Lang i = CPA i i | CPAI i i | JM i | JLR i i i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Eq (HContents st), RWValue (Address v) (Heap st) (HContents st), 
  Incr v, Incr (HContents st), Decr v,
  HasQ st, HasHeap st, Q st ~ Address v) =>
  Lang v -> st -> st
trans (CPA r1 r2)        = copyMem (A r1) (A r2)
trans (CPAI r1 r2)       = previousQ . incrMem (A r2) . copyMem (A r1) (A r2) 
trans (JM a1)            = jjump (A a1)
trans (JLR r1 r2 a1 a2)  = jumpe2 heap (A r1) heap (A r2) (A a1) (A a2)
trans HALT               = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CPA 0 1 , CPAI 0 1, JLR 0 1 3 3, HALT]
mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
