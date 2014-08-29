{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.RAM3 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), ModalAddress(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- Register to register model of Cook and Rechhow (1973)
-- Cook and Reckhow
data Lang i = LOAD i i | ADD i i i | SUB i i i | COPY (ModalAddress i) (ModalAddress i) | JNZ i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Ord (HContents st), Zero (HContents st),
  Subtractive (HContents st),
  RWValue (ModalAddress (HContents st)) (Heap st) (HContents st),
  Incr (HContents st), Additive (HContents st), HasQ st, HasHeap st,
  Q st ~ Address (HContents st)) => Lang (HContents st) -> st -> st  
trans (LOAD c r)     = lda heap (MADirect r) c
trans (ADD r1 r2 rd) = raddMem (MADirect r1) (MADirect r2) (MADirect rd)
trans (SUB r1 r2 rd) = rsubMem (MADirect r1) (MADirect r2) (MADirect rd)
trans (COPY r1 rd)   = copyMem r1 rd
trans (JNZ r z)      = stateful_If (cmp0 (>) heap (MADirect r)) (jjump (A z)) stepQ
trans (HALT)     = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [LOAD 3 0, ADD 0 0 1, SUB 1 2 0, COPY (MAIndirect 0) (MADirect 0), JNZ 0 1, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
