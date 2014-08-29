{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Defn (
    IsFSA, atEnd
    ) where

-- import Basic.Types
import Basic.Memory (SetLike(isElem))
import Basic.Features (HasState(..), HasTransition, HasInput, 
  HasFinal(..), HasQ(..))

import Control.Lens ((^.))

class (HasState x, HasTransition x, HasFinal x, HasInput x) => IsFSA x where

atEnd :: (Eq (Q (State s)), HasState s, HasQ (State s), HasFinal s,
  SetLike t, Final s ~ t (Q (State s))) => 
  s -> Bool
atEnd mach = isElem (mach^.state^.q) (mach^.final)
