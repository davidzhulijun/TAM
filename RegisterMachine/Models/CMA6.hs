{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA6 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import Basic.Operations (reify)
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.6. 1961: another example with two instructions, just single register
-----------------------------------------
data Lang i = MULTJM i i | DIVJM i i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False
  
trans :: (Integral (AContents c), Zero (AContents c),
  RWValue IMM (ACC c) (AContents c), Multiplicative (AContents c),
  Incr (AContents c), Divisible (AContents c), HasQ c, HasACC c,
  Q c ~ Address (AContents c)) => Lang (AContents c) -> c -> c
trans (MULTJM k a) = jjump (A a) . rmul acc IMM (reify k) IMM acc IMM 
trans (DIVJM k a1 a2)  = stateful_If (cmp0 (==) acc IMM) (jjump (A a2)) (jjump (A a1)) . rdiv acc IMM (reify k) IMM acc IMM
trans HALT     = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [MULTJM 1 1, DIVJM 1 1 2, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
