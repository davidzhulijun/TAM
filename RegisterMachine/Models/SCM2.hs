{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.SCM2 (
  mach, mmach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, MapMem, mapFromList, SingleLoc, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine

--4.1.2 Base model 0
data Lang i = INC | Halt | CLR | JE i i deriving Eq
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False
 
trans :: (Zero (HContents st), RWValue IMM (Heap st) (HContents st),
  Incr (HContents st), HasQ st,
  HasHeap st, Q st ~ Address (HContents st)) =>
  Lang (HContents st) -> st -> st  
trans INC      = incrMem IMM
trans Halt     = halt
trans CLR      = clearMem IMM
trans (JE v z) = jumpImm v (A z)
--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CLR, INC, INC, JE 1 1, JE 2 2, CLR, INC, JE 2 6, Halt]

mprog :: MapMem Int (Lang Int)
mprog = mapFromList 0 [CLR,INC,INC,JE 1 1,JE 2 2,CLR,INC,JE 2 6,Halt]

mach :: RM1 (Lang Int) ListMem (CounterMachineState Int SingleLoc (Address Int))
mach = RM prog initCM (compile trans)

mmach :: RM1 (Lang Int) (MapMem Int) (CounterMachineState Int SingleLoc (Address Int))
mmach = RM mprog initCM (compile trans)
