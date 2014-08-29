{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CM2 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
--4.1.2 Base model 2
data Lang i = INC i | Halt | CLR i | JE i i i
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False
  
trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st),
  HasQ st, HasHeap st, Q st ~ Address v) =>
  Lang v -> st -> st
trans (INC r)      = incrMem (A r)
trans Halt         = id
trans (CLR r)      = clearMem (A r)
trans (JE r1 r2 z) = jumpe2Mem (A r1) (A r2) (A z)

prog :: ListMem (Lang Int)
prog = fillMem [CLR 2, INC 3, INC 1, JE 0 1 1, JE 0 1 2, CLR 1, INC 2, JE 0 2 6, Halt]

mach :: RM1 (Lang Int) ListMem (CounterMachineState Int ListMem (Address Int))
mach = RM prog initCM (compile trans)
