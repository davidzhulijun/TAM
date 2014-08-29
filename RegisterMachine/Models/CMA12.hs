{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA12 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.12. 1963: Shepherdson and Sturgis's model
--         Limited Register Machine LRM:
-----------------------------------------
-- Todo it later

-- 4.0.13. 1963: Shepherdson and Sturgis's model
--         Single Register Machine SRM:
-----------------------------------------
-- Todo it later

-- 4.0.14. 1967: Minsky's simple universal base for a program computer
-----------------------------------------
-- First have Halt Function
-- What is the difference of  operation f&j
data Lang i = CLR i | INC i | JZ i i | JNZ i i | CPY i i | JM i | JNE i i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False
  
trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st),
  Decr (HContents st), HasQ st, HasHeap st,
  Q st ~ Address v) =>
  Lang v -> st -> st
trans (CLR r)  = clearMem (A r)
trans (INC r)  = incrMem (A r)
trans (JZ r a)  = jumpe0Mem (A r) (A a)
trans (JNZ r a)= stateful_If  (cmpne0 heap (A r)) (jjump (A a) . decrMem (A r)) stepQ
trans (CPY r d) = copyMem (A r) (A d)
trans (JM a) = jjump (A a)
trans (JNE r1 r2 a) = jumpne2Mem (A r1) (A r2) (A a)
trans HALT    = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CLR 0, INC 0, JZ 0 2, JNZ 1 2, CPY 0 1, JM 7,  JNE 0 1 7, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
