{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Models.CMA9 (
  Lang(..), prog, mach, trans
  ) where

import Basic.Types
import Basic.MemoryImpl (ListMem, fillMem, Address(..), SingleLoc)
import Basic.Features
import RegisterMachine.State
import RegisterMachine.Operations
import RegisterMachine.Machine
-- 4.0.9. 1961: Lambek abacus model: atomizing Melzak's model to X+, X- with test
--              Abacus model of Boolos-Burgess(1970), Boolos-Burgess-Jeffrey(2002) just single register
-----------------------------------------
data Lang i = ADDJM i | SUBJM i i | HALT
instance Language (Lang i)

instance IsHalt (Lang i) where
  isHaltInstr HALT = True
  isHaltInstr _    = False

trans :: (Ord (AContents c), Zero (AContents c), Incr v, Incr (AContents c),
  RWValue IMM (ACC c) (AContents c),
  Decr (AContents c), HasQ c, HasACC c, Q c ~ Address v) =>
  Lang v -> c -> c  
trans (ADDJM a) = jjump (A a) . incrACC 
trans (SUBJM a1 a2)  = stateful_If (cmp0 (<=) acc IMM) (jjump (A a2)) (jjump (A a1) . decrACC)
trans HALT    = id

--------------------------------------------------------------------------
--------------------------specilized examples
--------------------------------------------------------------------------
prog :: ListMem (Lang Int)
prog = fillMem [ADDJM 1, ADDJM 2, ADDJM 3, SUBJM 3 4, HALT]

mach :: RM1 (Lang Int) ListMem (CMAState Int SingleLoc ListMem (Address Int))
mach = RM prog initedCMA (compile trans)
