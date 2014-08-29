{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.RAM2 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), ModalAddress(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine

import Control.Lens ((^.))

-- Schonhage's RAM0 and RAM1 (1980)
-- RAM0 model from http://en.wikipedia.org/wiki/Random-access_machine
data Lang i = INCA | CLRA | LDA | JAZ i | STA i | CPYA2M i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Zero (AContents s), RWValue IMM (ACC s) (AContents s),
  SaveValue (ModalAddress v) (Heap s) (HContents s), Incr v,
  Incr (AContents s), HasHeap s, HasACC s, Q s ~ Address v,
  GetValue (ModalAddress (AContents s)) (Heap s) (HContents s), HasQ s,
  HContents s ~ AContents s) =>
  Lang v -> s -> s  
trans (INCA)  = incrACC
trans (CLRA)     = clearACC
trans (CPYA2M r2) = copyA2M (MADirect r2)
trans (LDA)     = \x -> copyM2A (MADirect (readMem IMM $ x^.acc)) x
trans (STA r2)   = copyA2M (MAIndirect r2)
trans (JAZ z)    = jumpe0ACC (A z)
trans (HALT) = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [INCA, LDA, CLRA, INCA, JAZ 2, INCA, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
