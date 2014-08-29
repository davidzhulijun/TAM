{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CM1 (
  mach1, mach2
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, MapMem, mapFromList, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine

---------------------------------------------------------
---------------- Counter Machine with list of Data register
---------------------------------------------------------
--4.1.1 Base model 1
data Lang i = DEC i | INC i | Halt | JZ i i
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False

trans :: (Zero (HContents st), RWValue (Address v) (Heap st) (HContents st),
  Incr v, Incr (HContents st), Decr (HContents st), HasQ st, HasHeap st,
  Q st ~ Address v) => Lang v -> st -> st
trans (INC r)  = incrMem (A r)
trans (DEC r)  = decrMem (A r)
trans Halt     = id
trans (JZ r z) = jumpe0Mem (A r) (A z)

-- Counter Machine without ACC
prog1 :: ListMem (Lang Int)
prog1 = fillMem [INC 2, DEC 2, INC 3, INC 1, INC 0, DEC 2, DEC 1, INC 0, INC 2, JZ 2 10, Halt]

prog2 :: MapMem Int (Lang Int)
prog2 = mapFromList 0 [JZ 1 3, INC 2, DEC 2, INC 3, INC 1, INC 0, DEC 2, DEC 1, INC 0, INC 2, JZ 2 9, Halt]

mach1 :: RM1 (Lang Int) ListMem (CounterMachineState Int ListMem (Address Int))
mach1 = RM prog1 initCM (compile trans)

mach2 :: RM1 (Lang Int) (MapMem Int) (CounterMachineState Int ListMem (Address Int))
mach2 = RM prog2 initCM (compile trans)
