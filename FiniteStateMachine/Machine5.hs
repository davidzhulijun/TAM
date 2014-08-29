{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Machine5 (
    FSM(FSM), compile, FSM1, outputcompile, 
    FSMState, initialFSMState
    ) where

import Basic.Types
import Basic.Memory
import Basic.Features
import Basic.Operations
import Control.Lens(makeLenses,(^.))
import FiniteStateMachine.State

--------------------------------------------------------------------------
---------------------------- Finite State Machine
--------------------------------------------------------------------------
data FSM s f i o outr = 
  FSM { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans (f, i) s
      , _inpt       :: i 
      , _outpt      :: o
      , _outtrans   :: outr}
makeLenses ''FSM

instance HasFinal (FSM s f i o outr) where
  type Final (FSM s f i o outr) = f
  final = finalstates

instance HasTransition (FSM s f i o outr) where
  transition = trans

instance HasInput (FSM s f i o outr) where
  type Input (FSM s f i o outr) = i
  input = inpt

instance HasOutput (FSM s f i o outr) where
  type Output (FSM s f i o outr) = o
  output = outpt

instance HasOutputTransition (FSM s f i o outr) where
  type OutputTransition (FSM s f i o outr) = outr
  outputtransition = outtrans

instance HasState (FSM s f i o outr) where
  type State (FSM s f i o outr) = s
  state = sta

instance HasStatic (FSM s f i o outr) where
  type Static (FSM s f i o outr) = (f, i)
  static g (FSM s f tr i o outr) = fmap (\(a,b) -> FSM s a tr b o outr) (g (f,i))

type FSM1 qs i c o ptr = FSM (FSMState qs ptr) (c qs) (c i) (c o) (Transout (c qs, c i) (FSMState qs ptr) (c o))

instance (SetLike c, Eq qs, AtEnd ptr (c i)) => 
    EndCond (FSM1 qs i c o ptr) where
  endcond m = isInputEnd m

outputcompile :: (GetValue (IP b) mem a, Store c, HasQ b, HasIP b) =>
  (a -> Q b -> d) -> Transout (t, mem a) b (c d)
outputcompile f = Transout g
  where g (_, i) st o = store (f (readMem (st^.ip) i) (st^.q)) o
