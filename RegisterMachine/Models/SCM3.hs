{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.SCM3 (
  Lang(..), prog, mprog, mach, mmach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, MapMem, mapFromList, SingleLoc, Address(..))
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
--4.1.3 Base model 3
data Lang i = INC | CP i | JE i i | Halt deriving Eq
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr Halt = True
  isHaltInstr _    = False

trans :: (Eq (HContents st), RWValue IMM (Heap st) (HContents st),
  Incr (HContents st), HasQ st,
  HasHeap st, Q st ~ Address (HContents st)) =>
  Lang (HContents st) -> st -> st
trans INC       = incrMem IMM
trans Halt      = halt
trans (CP r2)   = lda heap IMM r2
trans (JE r1 z) = jumpImm r1 (A z)
--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [INC , CP 2, INC , INC , JE 1 2, INC , Halt]

mprog :: MapMem Int (Lang Int)
mprog = mapFromList 0 [INC,CP 2,INC,INC,JE 1 2,INC,Halt]

mach :: RM1 (Lang Int) ListMem (CounterMachineState Int SingleLoc (Address Int))
mach = RM prog initCM (compile trans)

mmach :: RM1 (Lang Int) (MapMem Int) (CounterMachineState Int SingleLoc (Address Int))
mmach = RM mprog initCM (compile trans)
