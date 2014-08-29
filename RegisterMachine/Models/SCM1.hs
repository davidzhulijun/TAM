{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.SCM1 (
  mach, mmach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, MapMem, mapFromList, SingleLoc, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
---------------------------------------------------------
---------------- Counter Machine with single Data register
---------------------------------------------------------
--4.1.1 Base model 1
data Lang i = DEC | INC | Halt | JZ i deriving Eq
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False

trans :: (Zero (HContents st), RWValue IMM (Heap st) (HContents st), Incr v,
  Incr (HContents st), Decr (HContents st), HasQ st, HasHeap st,
  Q st ~ Address v) =>
  Lang v -> st -> st  
trans INC    = incrMem IMM
trans DEC    = decrMem IMM
trans Halt   = halt
trans (JZ z) = jumpe0Mem IMM (A z)
--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
-- Single Counter Machine
prog :: ListMem (Lang Int)
prog = fillMem [JZ 2, DEC, INC, INC, JZ 1, JZ 10, DEC, INC, JZ 0, Halt]

-- Counter Machine with map
mprog :: MapMem Int (Lang Int)
mprog = mapFromList 0 [JZ 2,DEC,INC,INC,JZ 1,JZ 10,DEC,INC,JZ 0,Halt]

mach :: RM1 (Lang Int) ListMem (CounterMachineState Int SingleLoc (Address Int))
mach = RM prog initCM (compile trans)

mmach :: RM1 (Lang Int) (MapMem Int) (CounterMachineState Int SingleLoc (Address Int))
mmach = RM mprog initCM (compile trans)
