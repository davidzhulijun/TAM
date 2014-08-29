{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.RAM1 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, ModalAddress(..), Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
--------------Macro Random Access Machine
-- Schonhage's RAM0 and RAM1 (1980)
-- RAM1 model from http://en.wikipedia.org/wiki/Random-access_machine
data Lang i = INCA | LDAC i | LDA (ModalAddress i) | JEA i i | STA (ModalAddress i) | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Eq (AContents st), RWValue IMM (ACC st) (AContents st),
  RWValue (ModalAddress (AContents st)) (Heap st) (HContents st),
  Incr (AContents st), HasQ st,
  HasHeap st, HasACC st, Q st ~ Address (AContents st),
  HContents st ~ (AContents st)) =>
  Lang (AContents st) -> st -> st
trans (INCA)  = incrACC
trans (LDAC r2)  = ldaACC r2
trans (LDA r2)   = copyM2A r2
trans (STA r2)   = copyA2M r2
trans (JEA r1 z) = jumpe2AccMem (MADirect r1) (A z) 
trans HALT      = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [INCA, LDAC 2, INCA, INCA, JEA 1 2, INCA, LDA (MAIndirect 2), LDA (MADirect 1), HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
