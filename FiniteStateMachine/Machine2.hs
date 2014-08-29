{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Machine2 (
    FSM(FSM), compile, FSM1
    ) where

import Basic.Types
import Basic.Memory
import Basic.Features
import Control.Lens(makeLenses,(^.))
import FiniteStateMachine.State2

--------------------------------------------------------------------------
---------------------------- Finite State Machine
--------------------------------------------------------------------------
data FSM s f i = 
  FSM { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans f (FSMState s i)
      , _inpt       :: i }
makeLenses ''FSM

instance HasFinal (FSM s f i) where
  type Final (FSM s f i) = f
  final = finalstates

instance HasTransition (FSM s f i) where
  transition = trans


instance HasState (FSM s f i) where
  type State (FSM s f i) = FSMState s i
  state g (FSM s f tr i) = fmap (\(FSMState a b) -> FSM a f tr b) (g (FSMState s i))

instance HasStatic (FSM s f i) where
  type Static (FSM s f i) = f
  static = finalstates

type FSM1 qs i c = FSM qs (c qs) (c i)

instance (SetLike c, Eq qs, IsEmpty c) => EndCond (FSM qs (c qs) (c i)) where
  endcond m = isEmpty (m^.state^.input)
