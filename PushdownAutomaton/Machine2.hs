{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module PushdownAutomaton.Machine2 (
  PDA(PDA), PDAn
  ) where 

import Basic.Types
import Basic.Memory --(SetLike(..),IsAtProgramEnd(..))
import Basic.Features
--import Basic.Operations
import PushdownAutomaton.State2

import Control.Lens

--class (HasState x, HasTransition x, HasFinal x) => HasPDA x where

--------------------------------------------------------------------------
--------------------- Pushdown Automata with 1 stack
--------------------------------------------------------------------------
data PDA s m f i = 
  PDA { _sta       :: s
      , _mem       :: m
      , _finalstates :: f
      , _trans       :: Trans f (PDAState s m i)
      , _inpt       :: i }
makeLenses ''PDA

instance HasState (PDA s m f i) where
  type State (PDA s m f i) = PDAState s m i
  state g (PDA s m f tr i) = fmap (\(PDAState a b c) -> PDA a b f tr c) (g (PDAState s m i)) 

instance HasTransition (PDA s m f i) where
  transition = trans

instance HasFinal (PDA s m f i) where
  type Final (PDA s m f i) = f
  final = finalstates

instance HasStatic (PDA s m f i) where
  type Static (PDA s m f i) = f
  static = finalstates

--------------------------------------------------------------------------
---------------------------------machine operations
---------------------------------------------------------------------------
type PDA1 s m c i = PDA s m (c s) (c i) 

instance (Eq qs, SetLike mem, IsEmpty mem) => 
  EndCond (PDA1 qs stck mem inp) where
  endcond m = isEmpty (m^.state^.input)

--------------------------------------------------------------------------
--------------------- Pushdown Automata with n stack
--------------------------------------------------------------------------
type PDAn q map n stack s c l i = PDA q (map n (stack s)) (c q) (l i)