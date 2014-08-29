{-# OPTIONS -Wall #-}

module TuringMachine.Models.TM4 (
  mach
  ) where

import Basic.Types (Pointed(..))
import Basic.MemoryImpl
import TuringMachine.State
import TuringMachine.Machine

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
-- DTM (Deterministic Turing Machine)
-- 1 Examples of Turing Machine
data CState = B | C | D | HALT deriving (Show, Eq)
data TMAlp = Zero | One  deriving (Show, Eq)
instance Pointed TMAlp where pt = Zero

-- Shouldn't this have up and down transitions?
trans :: CState -> [TMAlp] -> (CState, [TMAlp], [TMDirection2])
trans D [Zero] = (B, [One] , [TRight])
trans D [One]  = (C, [One] , [TLeft])
trans B [Zero] = (D, [One] , [TLeft])
trans B [One]  = (B, [One] , [TRight])
trans C [Zero] = (B, [One] , [TLeft])
trans C [One]  = (HALT, [One] , [TRight])
trans _ _ = error "undefined transition" 

initialState :: TMStaten IOne MapMem TMAlp TapeMem2 CState
initialState = initialTMStaten Zero D
finalState :: ListMem CState
finalState = fillMem [HALT]

mach :: TMn IOne MapMem TMAlp TapeMem2 CState ListMem
mach = TM initialState finalState (gcompile $ translate trans)

