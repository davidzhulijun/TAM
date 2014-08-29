{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -Wall #-}

module Basic.Types (
  Location, Pointed(..), 
  Incr(incr), Decr(decr),
  Additive(add), Subtractive(sub), Multiplicative(mul), Divisible(div2),
  One(one), Zero(zero),
  GetValue(..), SaveValue(..), RWValue, 
  IMM(..),
  Here(..),
  EndCond(..), 
  Forward(next), Backward(previous), BiDirectional,
  Up(up), Down(down), FourDirectional,
  Trans(Trans,getTrans), Transout(Transout,getTransout),
  Copointed(copoint),
  Language
) where

import Data.Word
default ()

-- allows one to declare locations, which will be 'in' memory
class Location y where

------------------------------------------------------------------------------
-- 0 Data Types & Operation
-- We have a distinguised point (that we can 'tell apart')
class Eq v => Pointed v where
    pt :: v

instance Pointed v => Pointed (v,v) where
    pt = (pt, pt)

-- value operations
class Incr v where
    incr :: v -> v

class Decr v where
    decr :: v -> v

class Additive v where
    add :: v -> v -> v

class Subtractive v where
    sub :: v -> v -> v

class Multiplicative v where
    mul :: v -> v -> v

class Divisible v where
    div2 :: v -> v -> v

-- Zero and One are different than 'Pointed'.  They do imply Eq though.
class Eq v => Zero v where
    zero :: v
class Eq v => One v where
    one :: v

-- 1 Data Operation Instances
-- 1.1 Data Operations
instance Pointed Int where
    pt = 0

instance Pointed Word8 where
    pt = 0

instance Incr Int where
    incr x = x + 1
instance Decr Int where
    decr x = x - 1
instance Additive Int where
    add a b = a + b
instance Subtractive Int where
    sub a b = a - b
instance Multiplicative Int where
    mul a b = a * b
instance Divisible Int where
    div2 a b = div a b
instance Zero Int where
    zero = 0
instance One Int where
    one = 1

instance Additive Word8 where
    add = (+)
instance Subtractive Word8 where
    sub = (-)

------------------------------------------------------------------------------
-- 1 Memory Types
-- The following essentially defines class-level lenses for focusing on a value
-- of a container

-- at location loc, in memory mem, store things of type a
class (Location loc) => GetValue loc mem a where
    readMem :: loc -> mem a -> a

-- given a location, returns a 'put' for that
class Location loc => SaveValue loc l a where
    writeMem :: loc -> l a -> a -> l a

-- Shorthand for a full (indexed) lens
type RWValue loc mem a = (GetValue loc mem a, SaveValue loc mem a)

data IMM = IMM
instance Location IMM

data Here k = Here k
instance GetValue IMM Here a where
  readMem IMM (Here k) = k

------------------------------------------------------------------------------
-- 3 Classes for 'running' machines
-- How to tell if we have reached the 'end' of the run
class EndCond config where
  endcond :: config -> Bool

------------------------------------------------------------------------------
-- 4 Transition 'function'

-- A transition function takes some datum 'd' and returns a 
-- state-to-state function.
newtype Trans d st = Trans {getTrans :: d -> st -> st}
newtype Transout a b c = Transout {getTransout :: a -> b -> c -> c}

---------------------------------------------------------

-- 5 Movement functions

-- move x forward
-- can be applied to state, input, etc.
-- 'x' is usually some kind of address or pointer
class Forward x where
    next :: x -> x

class Backward x where
    previous:: x -> x

class (Forward x, Backward x) => BiDirectional x where

class Up x where
    up :: x -> x

class Down x where
    down :: x -> x

class (Forward x, Backward x, Up x, Down x) => FourDirectional x where

------------------------------------------------------------------------------
-- 6 Handling input
-- | 'Copointed' does not require a 'Functor', as the only relationship
-- between 'copoint' and 'fmap' is given by a free theorem

class Copointed p where
  copoint :: p a -> a

------------------------------------------------------------------------------
-- For declaring what the languages are.  Useful for 'programs'.
class Language l where
