{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Basic.Memory (
  AtEnd(..), Store(..),
  SetLike(..), Empty(..), IsEmpty(..), Initialized(..),
  --Stack
  Stack(..),
  --Tape
  FixedTape(..), MoveTape(..),
  -- Map
  MapLike(..), InitMapLike(..), AscListLike(..)
    ) where

import Basic.Types

-- is loc at the end of (the memory) mem?
class AtEnd loc mem where
  isFinal :: loc -> mem -> Bool

class Store mem where
  store :: a -> mem a -> mem a

-- Empty contains 'nothing'
class Empty s where
  empty :: s v

-- Whereas this is "full" but of the underlying point
class Initialized s where
  inited :: Pointed v => s v

class IsEmpty s where
  isEmpty :: s v -> Bool

------------------------------------------------------------------------
-- Set
-------------------------------------------------------------------------

class SetLike s where
  isElem :: Eq v => v -> s v -> Bool

------------------------------------------------------------------------
-- Stack 
-------------------------------------------------------------------------

class (Empty s, IsEmpty s) => Stack s where
  push :: v -> s v -> s v
  top :: s v -> v
  pop :: s v -> s v
  initS :: [v] -> s v

------------------------------------------------------------------------
-- Tape 
-------------------------------------------------------------------------
class Initialized s => FixedTape s where
  taperead :: s v -> v
  tapewrite :: s v -> v -> s v

class FixedTape s => MoveTape s d where
  tapemove :: d -> s v -> s v

------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------
class MapLike m where
  lookupM :: Ord a => m a v -> a -> v
  emptyM :: m a v
  emptyM' :: (Ord a, Enum a, Bounded a, Empty v) => m a (v b)
  memberM :: Ord a => m a v -> a -> Bool
  insertM :: Ord a => a -> v -> m a v -> m a v
  findMaxM :: m a v -> (a,v)
  mapWithKeyM :: m a v -> (a -> v -> w) -> m a w

-- a Map whose underlying contents can be initialized
class MapLike m => InitMapLike m where
  initM :: (Ord a, Enum a, Bounded a, Initialized v, Pointed b) => m a (v b)

class MapLike m => AscListLike m where
  mapFromL :: (Enum i, Ord i) => i -> [v] -> m i v
  mapToL :: m i v -> [(i,v)]
