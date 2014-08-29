{-# OPTIONS -Wall #-}

module PushdownAutomaton.Operations (
  StackCommd(..), stackoperation
  ) where
  
import Basic.Memory --(Stack(..), empty, MapLike(..))

---------------------------------------------------------------------
-- Stack Operations
data StackCommd v = Push v | Pop | None

stackoperation :: Stack s => StackCommd v -> s v -> s v
stackoperation Pop      = pop
stackoperation (Push v) = push v
stackoperation None     = id