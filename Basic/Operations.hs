{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS -Wall #-}

module Basic.Operations (
    step, stepout, run, runout,
    isInputEnd, isAcceptState,
    reify
) where

import Basic.Types (Trans(..), Transout(..), EndCond(..), Here(..))
import Basic.Features (HasState(..), HasQ(..), HasFinal(..),
  HasTransition(..), HasOutputTransition(..), HasOutput(..), HasStatic(..),
  HasInput(..), HasIP(..))
import Basic.Memory (AtEnd, isFinal, SetLike(..))
import Control.Lens ((%~), (^.), Getter, to)

---------------------------------------------------------
--------------Machine Run

loop :: EndCond c => (c -> c) -> (c -> c)
loop f c = if endcond c then c else loop f $ f c 

step :: (HasTransition c) => c -> c
step c = state %~ (getTrans (c^.transition) (c^.static)) $ c

run :: (EndCond c, HasTransition c) => c -> c
run = loop step

-- With output
stepout :: (HasState c, HasOutputTransition c, HasOutput c, HasStatic c,
  OutputTransition c ~ Transout (Static c) (State c) (Output c)) => c -> c
stepout c = output %~ (getTransout (c^.outputtransition) (c^.static) (c^.state)) $ c

runout :: (EndCond c, HasTransition c, HasOutputTransition c, HasOutput c,
  OutputTransition c ~ Transout (Static c) (State c) (Output c)) =>
  c -> c
runout = loop (stepout . step)

---------------------------------------------------------
------structure with state record and IP
isInputEnd :: (HasState s, HasInput s, HasIP (State s),
  AtEnd (IP (State s)) (Input s)) => s -> Bool
isInputEnd c = isFinal (c^.state^.ip) (c^.input)

isAcceptState :: (Eq (Q (State s1)), HasState s1, HasQ (State s1), HasFinal s1,
  SetLike s, Final s1 ~ s (Q (State s1))) =>
  s1 -> Bool
isAcceptState c = isElem (c^.state^.q) (c^.final)

---------------------------------------------------------
reify :: k -> Getter x (Here k)
reify = to . const . Here
