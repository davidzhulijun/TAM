{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA7 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine

-- 4.0.7. 1961: Melzak model: a single ternary instruction with addition and proper subtraction
-- Successor machine
-----------------------------------------
data Lang i = CLR i | INC i | JE i i i |HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st),
  HasQ st, HasHeap st, Q st ~ Address v) =>
  Lang v -> st -> st  
trans (CLR r) = clearMem (A r) 
trans (INC r) = incrMem (A r) 
trans (JE r1 r2 a)  = jumpe2Mem (A r1) (A r2) (A a)
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CLR 0, INC 0, JE 0 1 1, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
