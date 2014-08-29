{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module FiniteStateMachine.Machine4 (
    FSM(FSM), compile, FSM1, outputcompile
    ) where

import Basic.Types
import Basic.Memory
import Basic.Features
import Control.Lens(makeLenses,(^.))
import FiniteStateMachine.State2
--import Control.Monad.Writer

--------------------------------------------------------------------------
---------------------------- Finite State Machine
--------------------------------------------------------------------------
data FSM s f i o outr = 
  FSM { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans f (FSMState s i)
      , _inpt       :: i 
      , _outpt      :: o
      , _outtrans   :: outr}
makeLenses ''FSM

instance HasFinal (FSM s f i o outr) where
  type Final (FSM s f i o outr) = f
  final = finalstates

instance HasTransition (FSM s f i o outr) where
  transition = trans

instance HasOutput (FSM s f i o outr) where
  type Output (FSM s f i o outr) = o
  output = outpt

instance HasOutputTransition (FSM s f i o outr) where
  type OutputTransition (FSM s f i o outr) = outr
  outputtransition = outtrans

instance HasState (FSM s f i o outr) where
  type State (FSM s f i o outr) = FSMState s i
  state g (FSM s f tr i o outr) = fmap (\(FSMState a b) -> FSM a f tr b o outr) (g (FSMState s i))

instance HasStatic (FSM s f i o outr) where
  type Static (FSM s f i o outr) = f
  static = finalstates

type FSM1 qs i c o = FSM qs (c qs) (c i) (c o) (Transout (c qs) (FSMState qs (c i)) (c o))

instance (SetLike c, Eq qs, IsEmpty c) => EndCond (FSM1 qs i c o) where
  endcond m = isEmpty (m^.state^.input)

--outputcompile :: (Value i -> qs -> Value o) -> Trans (FSMState qs i) o


outputcompile :: (Copointed i, Store o) => (a -> qs -> b) -> Transout d (FSMState qs (i a)) (o b)
outputcompile f = Transout g
  where g _ st o = store (f (copoint (st^.input)) (st^.q)) o
