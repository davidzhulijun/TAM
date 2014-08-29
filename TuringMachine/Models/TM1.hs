{-# OPTIONS -Wall #-}

module TuringMachine.Models.TM1 (
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

instance Pointed TMAlp where pt = Zero -- ??  What is the Blank symbol?
trans :: CState -> [TMAlp] -> (CState, [TMAlp], [TMDirection])
trans D [Zero] = (B, [One] , [TMRight])
trans D [One]  = (C, [One] , [TMLeft])
trans B [Zero] = (D, [One] , [TMLeft])
trans B [One]  = (B, [One] , [TMRight])
trans C [Zero] = (B, [One] , [TMLeft])
trans C [One]  = (HALT, [One] , [TMRight])
trans _ _ = error "undefined transition" 

initialState :: TMStaten IOne MapMem TMAlp TapeMem CState
initialState = initialTMStaten Zero D
finalState :: ListMem CState
finalState = fillMem [HALT]

mach :: TMn IOne MapMem TMAlp TapeMem CState ListMem
mach = TM initialState finalState (gcompile $ translate trans)
