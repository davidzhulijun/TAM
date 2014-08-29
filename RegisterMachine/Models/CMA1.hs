{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA1 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
-- A lot of the following is taken from 
-- http://en.wikipedia.org/wiki/Counter_machine
-- and http://en.wikipedia.org/wiki/Counter-machine_model
--------------------------------------------------------------------------------------------------------------
-- Counter Machine Model in history
-- 4.0.1. 1954: Hermes' model
-----------------------------------------
data Lang i = CPM2A i | CPA2M i | CLRA | INCA | JZA i | ONA i | ONEA | HALTA
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALTA = True
  isHaltInstr _    = False

-- below A is used instead of MADirect since no MAIndirect is used;  need to discuss this.
trans :: (Zero (HContents st), SaveValue (Address v) (Heap st) (HContents st),
  SaveValue IMM (ACC st) (AContents st), One (HContents st), Incr v,
  Incr (HContents st), GetValue (Address v) (Heap st) (HContents st),
  GetValue IMM (ACC st) (AContents st), HasQ st, HasHeap st, HasACC st,
  Q st ~ Address v, (AContents st) ~ (HContents st)) =>
  Lang v -> st -> st
trans (CPM2A r)  = copyM2A (A r)
trans (CPA2M r)  = copyA2M (A r)
trans (CLRA)    = clearACC
trans (INCA)    = incrACC
trans (JZA a)   = jumpe0ACC (A a)
trans (ONA r)   = stateful_If (cmpe2 acc IMM heap (A r)) clearACC  (ldaACC one)
trans (ONEA)    = ldaACC one
trans HALTA     = halt

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [CPM2A 1, CPA2M 2, INCA, JZA 1, CLRA, ONA 1, ONEA, HALTA]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
