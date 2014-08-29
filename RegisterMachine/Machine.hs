{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Machine (
    RM(RM), IsHalt(isHaltInstr), RM1
	) where

import Basic.Types (Trans, EndCond(endcond), GetValue(readMem), Language)
import Basic.Features (HasState(..), HasTransition(..), HasProgram(..),
  HasStatic(..), HasQ(..), IsAtProgramEnd(..), IsHalt(..))
import Control.Lens ((^.), makeLenses)

--------------------------------------------------------------------------
---------------------------- Generic Register Machine
--------------------------------------------------------------------------
-- Machine Definition
-- instr - type of language of instructions
-- c - container type for sequence of instructions (aka program)
-- st - type of states
data RM prg st =
    RM { _prog :: prg
       , _stat :: st
       , _trans :: Trans prg st}
makeLenses ''RM

instance HasState (RM prg st) where
  type State (RM prg st) = st
  state = stat

instance HasTransition (RM prg st) where
  transition = trans

instance HasProgram (RM prg st) where
  type Program (RM prg st) = prg
  program = prog

instance HasStatic (RM prg st) where
  type Static (RM prg st) = prg
  static = prog
--------------------------------------------------------------------------
----------------------------------generic machine operations
--------------------------------------------------------------------------

instance (IsAtProgramEnd (Q st) l, GetValue (Q st) l instr, Language instr,
  IsHalt instr,  HasQ st) => EndCond (RM (l instr) st) where
  endcond mach = isAtProgramEnd addr p || isHaltInstr (readMem addr p) where
    p = mach^.program
    addr = mach^.state^.q

type RM1 instr c st = RM (c instr) st
