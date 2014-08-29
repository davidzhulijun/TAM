{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.State2 (
  FSMState(FSMState), 
  FSMTrans, compile
  ) where

import Basic.Types
import Basic.Features
import Control.Lens

-- 1.1  acceptor for finite state machine 
data FSMState qs i = FSMState {_cs :: qs, _inp :: i}
makeLenses ''FSMState

instance HasQ (FSMState qs i) where
  type Q (FSMState qs i) = qs
  q = cs

instance HasInput (FSMState qs i) where
  type Input (FSMState qs i) = i
  input = inp

type FSMTrans a qs = a -> qs -> qs

compile :: (Forward (m a), Copointed m) => (a -> qs -> qs) -> Trans d (FSMState qs (m a))
compile f = Trans g
  where g _ (FSMState qs i) = FSMState (f (copoint i) qs) (next i)
