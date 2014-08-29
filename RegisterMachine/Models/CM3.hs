{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CM3 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
--4.1.3 Base model 3
data Lang i = INC i | CP i i | JE i i i | Halt
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False

trans :: (Eq (HContents st), RWValue (Address v) (Heap st) (HContents st), 
  Incr v, Incr (HContents st), HasQ st, HasHeap st, Q st ~ Address v) =>
  Lang v -> st -> st  
trans (INC r)      = incrMem (A r)
trans Halt         = halt
trans (CP r1 r2)   = copyMem (A r1) (A r2)
trans (JE r1 r2 z) = jumpe2Mem (A r1) (A r2) (A z)

prog :: ListMem (Lang Int)
prog = fillMem [INC 0, CP 0 2, INC 3, INC 0, JE 0 1 2, INC 1, Halt]

mach :: RM1 (Lang Int) ListMem (CounterMachineState Int ListMem (Address Int))
mach = RM prog initCM (compile trans)
