{-# OPTIONS -Wall #-}

module TuringMachine.Models.TM5 (
  mach
  ) where

import Basic.Types (Pointed(..))
import Basic.MemoryImpl
import TuringMachine.State
import TuringMachine.Machine

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
-- MTM (Multidimensional Turing Machine)
-- 1 Examples of Turing Machine
data CState = B | C | D | HALT deriving (Show, Eq)
data TMAlp = Zero | One  deriving (Show, Eq)

instance Pointed TMAlp where pt = Zero

trans :: CState -> [TMAlp] -> (CState, [TMAlp], [TMDirection])
trans D [Zero, Zero] = (B, [One, One] , [TMRight, TMRight])
trans D [One , One ] = (C, [One, One] , [TMLeft, TMLeft])
trans B [Zero, Zero] = (D, [One, One] , [TMLeft, TMLeft])
trans B [One , One ] = (B, [One, One] , [TMRight, TMRight])
trans C [Zero, Zero] = (B, [One, One] , [TMLeft, TMLeft])
trans C [One , One ]  = (HALT, [One, One] , [TMRight, TMRight])
trans _ _ = error "undefined transition" 

initialState :: TMStaten ITwo MapMem TMAlp TapeMem CState
initialState = initialTMStaten Zero D
finalState :: ListMem CState
finalState = fillMem [HALT]

mach :: TMn ITwo MapMem TMAlp TapeMem CState ListMem
mach = TM initialState finalState (gcompile $ translate trans)
