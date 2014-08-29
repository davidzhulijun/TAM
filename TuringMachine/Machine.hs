{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS -Wall #-}

module TuringMachine.Machine (
	TM(TM), TMn
	) where

import Basic.Types
import Basic.Memory (SetLike(..))
import Basic.Operations
import Basic.Features
import TuringMachine.State

import Control.Lens

--------------------------------------------------------------------------
---------------------------- Turing Machine
--------------------------------------------------------------------------
data TM s f = 
  TM { _sta       :: s
     , _finalstates :: f
     , _trans       :: Trans f s }
makeLenses ''TM

instance HasState (TM s f) where
  type State (TM s f ) = s
  state = sta

instance HasTransition (TM s f) where
  transition = trans

instance HasFinal (TM s f) where
  type Final (TM s f) = f
  final = finalstates

instance HasStatic (TM s f) where
  type Static (TM s f) = f
  static g (TM s f tr) = fmap (\a -> TM s a tr) (g f)

instance (Eq q, SetLike l)
  => EndCond (TM (TMState tap q) (l q)) where
  endcond mach = isAcceptState mach

--------------------------------------------------------------------------
--------------------------Turing Machine with 1 tape and 1 threads
---------------------------------------------------------------------------
--type TM1 t tape q c = TM (TMState1 t tape q) (c q)

--------------------------------------------------------------------------
---------------------------- Turing Machine with 2 tape and 2 threads
--------------------------------------------------------------------------
--type TM2 t1 t2 tape q c = TM (TMState2 t1 t2  tape q) (c q)

--------------------------------------------------------------------------
---------------------------- Turing Machine with n tape and n threads
--------------------------------------------------------------------------
type TMn n map t tape q c = TM (TMStaten n map t tape q) (c q)
