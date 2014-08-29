{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS -Wall #-}

module Basic.Features (
  HasMemory(..), HasQ(..), HasIP(..),
  HasHeap(..), HasACC(..),
  HasState(..), HasFinal(..), HasTransition(..), HasInput(..),
  HasStatic(..), HasOutput(..), HasOutputTransition(..),
  IsHalt(isHaltInstr), IsAtProgramEnd(isAtProgramEnd),
  HasProgram(..)) where

import Basic.Types (Trans, Language)
import Control.Lens

-----------------------------------------------------------------------------------------------
-- 2. Machine Features
-----------------------------------------------------------------------------------------------
-- State is a subset of Features (such as {Memory, Q})

class HasState x where
  type State x :: *
  state :: Simple Lens x (State x)

class (HasStatic x, HasState x) => HasTransition x where
  transition :: Getter x (Trans (Static x) (State x))

class HasOutputTransition x where
  type OutputTransition x :: *
  outputtransition :: Getter x (OutputTransition x)

class HasStatic x where
  type Static x :: *
  static :: Getter x (Static x)

-----------------------------------------------------------------------------------------------
-- 3. State Features
-----------------------------------------------------------------------------------------------
-- 3.1 Memory Features
-- a Memory is a feature of a state too
class HasMemory x where
  type Memory x :: *
  memory :: Simple Lens x (Memory x)

class HasHeap x where
  type Heap x :: * -> *
  type HContents x :: *
  heap :: Simple Lens x ((Heap x) (HContents x))

-- An accumulator is a degenerate Heap with a single cell.
-- So acc is a synonym for heap, but with only IMM instances for Get/Set
class HasACC x where
  type ACC x :: * -> *
  type AContents x :: *
  acc :: Simple Lens x ((ACC x) (AContents x))

-- 3.2 Control Features
-- The 'current state', frequently called q in state machines and 
-- Instruction Register for register machines.  All unified here in Q.

class HasQ x where
  type Q x :: *
  q :: Simple Lens x (Q x)

-----------------------------------------------------------------------------------------------
-- 4. Static Features
-----------------------------------------------------------------------------------------------

-- note how this is a Getter, not a Simple Lens.
class HasFinal x where
  type Final x :: *
  final :: Getter x (Final x)

class HasProgram x where
  type Program x :: *
  program :: Getter x (Program x)

-----------------------------------------------------------------------------------------------
-- 5. Input Features
-----------------------------------------------------------------------------------------------

class HasInput x where
  type Input x :: *
  input :: Getter x (Input x)

class HasIP x where
  type IP x :: *
  ip :: Simple Lens x (IP x)

-----------------------------------------------------------------------------------------------
-- 6. Output Features
-----------------------------------------------------------------------------------------------

-- This really should be a Setter, not a full lens.
class HasOutput x where
  type Output x :: *
  output :: Simple Lens x (Output x)

-----------------------------------------------------------------------------------------------
-- 7. Program Features
-----------------------------------------------------------------------------------------------

class IsHalt instr where
  isHaltInstr :: instr -> Bool

-- is loc at the end of the program (stored in memory mem)?
class IsAtProgramEnd loc mem where
  isAtProgramEnd :: Language l => loc -> mem l -> Bool
