{-# OPTIONS -Wall #-}

module TuringMachine.Models.TM2 (
  mach
  ) where

import Basic.Types (Pointed(..))
-- import Basic.Memory
import Basic.MemoryImpl
import TuringMachine.State
import TuringMachine.Machine

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
-- MTM (Multitape Turing Machine) with same thread
-- 1 Examples of Turing Machine
data CState = B | C | D | HALT deriving (Show, Eq)
data TMAlp = Zero | One  deriving (Show, Eq)

instance Pointed TMAlp where pt = Zero

-- trans :: TMTransn CState (TMAlp, TMAlp) TMDirection
trans :: CState -> [(TMAlp, TMAlp)] -> (CState, [(TMAlp, TMAlp)], [TMDirection])
trans D [(Zero, Zero)] = (B, [(One, One)] , [TMRight])
trans D [(One , One )] = (C, [(One, One)] , [TMLeft])
trans B [(Zero, Zero)] = (D, [(One, One)] , [TMLeft])
trans B [(One , One )] = (B, [(One, One)] , [TMRight])
trans C [(Zero, Zero)] = (B, [(One, One)] , [TMLeft])
trans C [(One , One )]  = (HALT, [(One, One)] , [TMRight])
trans _ _ = error "undefined transition" 

initialState :: TMStaten IOne MapMem (TMAlp, TMAlp) TapeMem CState
initialState = initialTMStaten (Zero, Zero) D
finalState :: ListMem CState
finalState = fillMem [HALT]

mach :: TMn IOne MapMem (TMAlp, TMAlp) TapeMem CState ListMem
mach = TM initialState finalState (gcompile $ translate trans)
