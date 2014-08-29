{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA13 (
  mach
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), ModalAddress(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine

import Control.Lens ((^.))

-- 4.0.15. 1980: Schonhage's 0-parameter model RAM0
-----------------------------------------
data Lang i = CLRA | INCA | CPYA i | CPYAI | STOAI i | JZA | JMA i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Zero (AContents s), SaveValue (ModalAddress v) (Heap s) (HContents s),
  RWValue IMM (ACC s) (AContents s), Incr v, Incr (AContents s),
  GetValue (ModalAddress (AContents s)) (Heap s) (HContents s),
  HasQ s, HasHeap s, HasACC s,
  Q s ~ Address v, HContents s ~ AContents s) =>
  Lang v -> s -> s
trans (CLRA )  = clearACC
trans (INCA )  = incrACC
trans (CPYA r)  = copyA2M (MADirect r)
trans (CPYAI )= \x -> copyM2A (MADirect (readMem IMM $ x^.acc)) x
trans (STOAI d) = copyA2M (MAIndirect d)
trans (JZA) = stepQ . stateful_If (cmpe0 acc IMM) stepQ id
trans (JMA a) = jjump  (A a)
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CLRA , INCA , CPYA 1, CPYAI , STOAI 1, JZA, JMA 7, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
