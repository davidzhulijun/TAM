{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA11 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.11. 1963: Shepherdson and Sturgis's model
--         Reduced Unlimited Register Machine URM:
-----------------------------------------
data Lang i = INC i | DEC i | JZR i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False
  
trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st), Decr (HContents st), HasQ st, HasHeap st,
  Q st ~ Address v) => Lang v -> st -> st
trans (INC r)  = incrMem (A r)
trans (DEC r)  = decrMem (A r)
trans (JZR r a)= jumpn0 heap (A r) (A a)
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [INC 0, DEC 0, JZR 0 3, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
