{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module PushdownAutomaton.Machine (
  PDA(PDA), PDAn
  ) where 

import Basic.Types (Trans, EndCond(endcond))
import Basic.Memory (SetLike(..), AtEnd(..))
import Basic.Features (HasState(..), HasTransition(..), HasStatic(..),
  HasInput(..), HasFinal(..))
import Basic.Operations (isInputEnd)
import PushdownAutomaton.State (PDAState, PDAStaten)

import Control.Lens

--------------------------------------------------------------------------
--------------------- Pushdown Automata with 1 stack
--------------------------------------------------------------------------
data PDA s f i = 
  PDA { _sta       :: s
      , _finalstates :: f
      , _trans       :: Trans (f,i) s
      , _inpt       :: i }
makeLenses ''PDA

instance HasState (PDA s f i) where
  type State (PDA s f i) = s
  state = sta

instance HasTransition (PDA s f i) where
  transition = trans

instance HasInput (PDA s f i) where
  type Input (PDA s f i) = i
  input = inpt

instance HasFinal (PDA s f i) where
  type Final (PDA s f i) = f
  final = finalstates

instance HasStatic (PDA s f i) where
  type Static (PDA s f i) = (f, i)
  static g (PDA s f tr i) = fmap (\(a,b) -> PDA s a tr b) (g (f,i))

--------------------------------------------------------------------------
---------------------------------machine operations
---------------------------------------------------------------------------

instance (Eq qs, SetLike mem, AtEnd i inp) => 
  EndCond (PDA (PDAState qs stck i) (mem qs) inp) where
  endcond = isInputEnd

--------------------------------------------------------------------------
--------------------- Pushdown Automata with n stacks
--------------------------------------------------------------------------
type PDAn q map n stack s vaddr c l i = PDA (PDAStaten q map n stack s vaddr) (c q) (l i)
