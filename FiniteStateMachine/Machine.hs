{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Machine (
    FSM(FSM), FSM1
    ) where

import Basic.Types
import Basic.Memory
import Basic.Features
import Basic.Operations
import FiniteStateMachine.State
--import FiniteStateMachine.Defn (atEnd)

import Control.Lens (makeLenses)

--------------------------------------------------------------------------
---------------------------- Finite State Machine
--------------------------------------------------------------------------
data FSM s f i = 
  FSM { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans (f, i) s
      , _inpt       :: i }
makeLenses ''FSM

-- The following is close to being automatable
instance HasState (FSM s f i) where
  type State (FSM s f i) = s
  state = sta

instance HasTransition (FSM s f i) where
  transition = trans

instance HasInput (FSM s f i) where
  type Input (FSM s f i) = i
  input = inpt

instance HasFinal (FSM s f i) where
  type Final (FSM s f i) = f
  final = finalstates

instance HasStatic (FSM s f i) where
  type Static (FSM s f i) = (f, i)
  static g (FSM s f tr i) = fmap (\(a,b) -> FSM s a tr b) (g (f,i))
--------------------------------------------------------------------------
---------------------------------machine operations
---------------------------------------------------------------------------
instance (Eq v, SetLike l, AtEnd a i) => EndCond (FSM (FSMState v a) (l v) i) where
  endcond m = isInputEnd m

-- convenient abbreviation
type FSM1 v a i c = FSM (FSMState v a) (c v) (c i)
