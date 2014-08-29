{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.State (
	FSMState(FSMState), initialFSMState,
  FSMTrans, compile
  ) where

import Basic.Types
import Basic.Features
import Control.Lens

-- 1.1  acceptor for finite state machine 
data FSMState qs ptr = FSMState {_cs :: qs, _inp :: ptr}
makeLenses ''FSMState

instance HasQ (FSMState qs ptr) where
  type Q (FSMState qs ptr) = qs
  q = cs

instance HasIP (FSMState qs ptr) where
  type IP (FSMState qs ptr) = ptr
  ip = inp

initialFSMState :: qs -> ptr -> FSMState qs ptr
initialFSMState = FSMState

type FSMTrans a qs = a -> qs -> qs

compile :: (GetValue ptr mem a, Forward ptr) =>
    (FSMTrans a qs) -> Trans (t, mem a) (FSMState qs ptr)
compile f = Trans g
  where g (_, inpt) (FSMState qs iloc) = FSMState qs' iloc'
          where ch = readMem (iloc) inpt
                qs' = f ch qs
                iloc' = next iloc
