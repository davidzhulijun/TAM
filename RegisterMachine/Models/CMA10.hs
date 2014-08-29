{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA10 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.10. 1963: Shepherdson and Sturgis's model
--         Unlimited Register Machine URM:
-----------------------------------------
data Lang i = INC i | DEC i | CLR i | CPY i i | JMP i | JZR i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False
  
trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st), Decr (HContents st), HasQ st, HasHeap st,
  Q st ~ Address v) => Lang v -> st -> st
trans (INC r)  = incrMem (A r)
trans (DEC r)  = decrMem (A r)
trans (CLR r)  = clearMem (A r)
trans (CPY r1 r2) = copyMem (A r1) (A r2)
trans (JMP a)  = jjump (A a)
trans (JZR r a)= jumpe0Mem (A r) (A a)
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [INC 0, DEC 0, CLR 0, CPY 0 1, JMP 6, JZR 0 6, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
