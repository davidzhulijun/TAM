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
import Basic.Operations
import Control.Lens(makeLenses)

--------------------------------------------------------------------------
---------------------------- Finite State Machine
--------------------------------------------------------------------------
data FSM s f i = 
  FSM { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans f (s, i)
      , _inpt       :: i }
makeLenses ''FSM

-- The following is close to being automatable
instance HasQ (FSM s f i) where
  type Q (FSM s f i) = s
  q = sta

instance HasFinal (FSM s f i) where
  type Final (FSM s f i) = f
  final = finalstates

instance HasTransition (FSM s f i) where
  transition = trans

instance HasInput (FSM s f i) where
  type Input (FSM s f i) = i
  input = inpt

instance HasState (FSM s f i) where
  type State (FSM s f i) = (s, i)
  state g (FSM s f tr i) = fmap (\(a, b) -> FSM a f tr b) (g (s, i))

instance HasStatic (FSM s f i) where
  type Static (FSM s f i) = f
  static = finalstates

compile :: (Forward m, HeadMem m) =>
  (Value m -> qs -> qs) -> Trans d (qs, m)
compile f = Trans g
  where g _ (qs, inp) = (f (focus inp) qs, next inp)

type FSM1 qs i c = FSM qs (c qs) (c i)

instance (SetLike c, Eq qs, IsAtProgramEnd (c i)) => 
    EndCond (FSM qs (c qs) (c i)) where
  endcond m = isProgramEnd m
