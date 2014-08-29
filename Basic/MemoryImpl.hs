{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module Basic.MemoryImpl (
  ModalAddress(..), Address(..),
  SingleLoc, ListMem(..), MapMem, StackMem, TapeMem, TapeMem2,
  TMDirection(..), TMDirection2(..), 
  emptySMem, initialSMem,
  emptyMem,  initialMem, fillMem,
  emptyMap, lookupMap, memberMap, insertMap, mapFromList, findMaxMap, mapToList,
  IOne, ITwo
    ) where

import Basic.Types
import Basic.Memory
import Basic.Features (IsAtProgramEnd(..))
import qualified Data.Map as Map
import Data.List
--import qualified Data.Array.IArray as Array
--import Data.Ix

-----------------------------------------------------------------------------------------------
-- 1. Memory Features
-----------------------------------------------------------------------------------------------
-- Register Address
-- Addresses: these are addresses into memory
-- Direct or Indirect. (only use when Indirect is needed)
data ModalAddress v = MADirect v | MAIndirect v
instance Location (ModalAddress v)

-- For (only) direct addressing
data Address v = A v deriving (Show, Eq)
instance Location (Address v)

instance Initialized Address where
    inited = A pt

instance (Incr a) => Forward (Address a) where
    next  (A i) = A (incr i)

instance (Decr a) => Backward (Address a) where
    previous (A i) = A (decr i)

instance (Incr a, Decr a) => BiDirectional (Address a) where

--------------------------------------------------------------------------
---------------------------- Level 2 Instance of basic registers
--------------------------------------------------------------------------

---------------------------------------------------------------------------  
-- a. single  memory location (store an accumulator, or a single register or ...)
---------------------------------------------------------------------------  

newtype SingleLoc v = L v

-- Single Data Register Operation

instance Initialized SingleLoc where
  inited = L pt

emptySMem :: Pointed v => SingleLoc v
emptySMem = L pt

initialSMem :: v -> SingleLoc v
initialSMem v = L v

-- Single Data Register Operation
instance GetValue IMM SingleLoc a where
    readMem IMM (L m) = m
instance SaveValue IMM SingleLoc a where
    writeMem IMM (L _) m = L m

---------------------------------------------------------------------------  
-- b. unbounded heap with slow access:
---------------------------------------------------------------------------  

newtype ListMem v = MemoryImpl [v]

emptyMem :: Pointed v => ListMem v
emptyMem = MemoryImpl $ repeat pt

initialMem :: v -> ListMem v
initialMem v = MemoryImpl $ repeat v

fillMem :: [v] -> ListMem v
fillMem = MemoryImpl

instance (Num i, Incr i, Eq i) => AtEnd (Address i) (ListMem v) where
  isFinal (A i) (MemoryImpl l) = genericLength l == incr i

instance (Num i, Incr i, Eq i) => IsAtProgramEnd (Address i) ListMem where
  isAtProgramEnd = isFinal

instance Copointed ListMem where
  copoint (MemoryImpl l) = head l

instance Store ListMem where
  store v (MemoryImpl l) = MemoryImpl (l++[v])

instance Forward (ListMem v) where
  next (MemoryImpl l) = MemoryImpl (tail l)

instance SetLike ListMem where
   isElem x (MemoryImpl l) = elem x l

instance IsEmpty ListMem where
   isEmpty (MemoryImpl []) = True
   isEmpty (MemoryImpl _)  = False

-- this is not the same as emptyMem !!!
instance Empty ListMem where
    empty = MemoryImpl []

-- but this is
instance Initialized ListMem where
    inited = emptyMem

-- ModalAddress with ListMem
instance Integral i => GetValue (ModalAddress i) ListMem i where
    readMem (MADirect i) (MemoryImpl l) = genericIndex l i
    readMem (MAIndirect i) l = let i' = readMem (MADirect i) l in readMem (MADirect i') l
instance Integral i => SaveValue (ModalAddress i) ListMem i where
    writeMem (MADirect i) (MemoryImpl l) d = (MemoryImpl $ genericTake i l ++ [d] ++ genericDrop (i+1) l)
    writeMem (MAIndirect i) l d = let i' = readMem (MADirect i) l in writeMem (MADirect i') l d

-- Address with ListMem
instance Integral i => GetValue (Address i) ListMem a where
    readMem (A i) (MemoryImpl  l) = genericIndex l i
instance Integral i => SaveValue (Address i) ListMem a where
    writeMem (A i) (MemoryImpl l) v = MemoryImpl $ genericTake i l ++ [v] ++ genericDrop (i+1) l

---------------------------------------------------------------------------  
-- c. map as Heap
---------------------------------------------------------------------------  

newtype MapMem a v = MapMemImpl (Map.Map a v) deriving Show

emptyMap :: MapMem i v
emptyMap = MapMemImpl $ Map.empty

instance (Ord i) => AtEnd (Address i) (MapMem i v) where
    isFinal (A i) (MapMemImpl m) = i == ind where
      (ind, _) = Map.findMax m

instance Ord i => IsAtProgramEnd (Address i) (MapMem i) where
  isAtProgramEnd = isFinal

-- cannot use plain lookup with a Map, instead:
lookupMap :: (Ord a) => MapMem a v -> a -> v
lookupMap (MapMemImpl m) a = 
    maybe (error "Attempting to access uninitialized memory")
          id (Map.lookup a m)

findMaxMap :: MapMem a v -> (a, v)
findMaxMap (MapMemImpl m) = Map.findMax m

memberMap :: (Ord a) => MapMem a v -> a -> Bool
memberMap (MapMemImpl m) a = Map.member a m

insertMap :: (Ord a) => a -> v -> MapMem a v -> MapMem a v
insertMap a v (MapMemImpl m) = MapMemImpl (Map.insert a v m)

mapFromList :: (Enum i, Ord i) => i -> [v] -> MapMem i v
mapFromList n l = MapMemImpl . snd $ 
    foldl (\(k,mp) v -> (succ k, Map.insert (toEnum k) v mp)) (fromEnum n, Map.empty) l

emptyMap' :: forall i v s. (Enum i, Bounded i, Ord i, Empty v) => MapMem i (v s)
emptyMap' = mapFromList minBound (map (const empty) [(minBound :: i)..maxBound])

initMap :: forall i v s. (Enum i, Bounded i, Ord i, Initialized v, Pointed s) => MapMem i (v s)
initMap = mapFromList minBound (map (const inited) [(minBound :: i)..maxBound])

mapWithKeyMap :: MapMem a v -> (a -> v -> w) -> MapMem a w
mapWithKeyMap (MapMemImpl m) f = MapMemImpl $ Map.mapWithKey f m

mapToList :: MapMem i v -> [(i,v)]
mapToList (MapMemImpl m) = Map.toAscList m

instance MapLike MapMem where
  emptyM = emptyMap
  emptyM' = emptyMap'
  lookupM = lookupMap
  memberM = memberMap
  insertM = insertMap
  findMaxM = findMaxMap
  mapWithKeyM = mapWithKeyMap

instance InitMapLike MapMem where
  initM = initMap

instance AscListLike MapMem where
  mapToL = mapToList
  mapFromL = mapFromList

--  Address with Map
instance (Integral i, Ord i) => GetValue (Address i) (MapMem i) a where
    readMem (A i) m = lookupMap m i
instance (Integral i, Ord i) => SaveValue (Address i) (MapMem i) a where
    writeMem (A i) (MapMemImpl m) v = MapMemImpl $ Map.insert i v m

-- ModalAddress with Map
instance (Integral i, Ord i) => GetValue (ModalAddress i) (MapMem i) i where
    readMem (MADirect i) m = lookupMap m i
    readMem (MAIndirect i) l = let i' = readMem (MADirect i) l in readMem (MADirect i') l
instance (Integral i, Ord i) => SaveValue (ModalAddress i) (MapMem i) i where
    writeMem (MADirect i) (MapMemImpl m) v = MapMemImpl $ Map.insert i v m
    writeMem (MAIndirect i) l v = let i' = readMem (MADirect i) l in writeMem (MADirect i') l v

---------------------------------------------------------------------------  
-- d. Array
---------------------------------------------------------------------------  
-- Array is strict in the bounds argument and in the indices of the association list,
-- but the values associated with indices that do not appear will be undefines.
{-
newtype ArrayMem a v = ArrayMemImpl (Array.Array a v)

initialArray :: (Pointed v, Integral i, Ix i)=> i -> ArrayMem i v
initialArray i = ArrayMemImpl (Array.listArray (0,i) [pt])

 This is fundamentally broken, put back in when fixed.

emptyArray :: (Pointed v, Integral i, Ix i)=> ArrayMem i v
emptyArray = ArrayMemImpl (Array.listArray (0,10) [pt])

-- 2.5.1.5 Address with Array
instance (Integral i, Ord i, Ix i) => GetValue (Address i) (ArrayMem i v) where
    readMem (A i) (ArrayMemImpl m) = m Array.! i
instance (Integral i, Ord i, Ix i) => SaveValue (Address i) (ArrayMem i v) where
    writeMem (A i) (ArrayMemImpl m) v = ArrayMemImpl $ m Array.// [(i, v)]

-- ModalAddress with Array
instance (Integral i, Ord i, Ix i) => GetValue (ModalAddress i) (ArrayMem i i) where
    readMem (MADirect i) (ArrayMemImpl m) = m Array.! i
    readMem (MAIndirect i) l = let i' = readMem (MADirect i) l in readMem (MADirect i') l
instance (Integral i, Ord i, Ix i) => SaveValue (ModalAddress i) (ArrayMem i i) where
    writeMem (MADirect i) (ArrayMemImpl m) v = ArrayMemImpl $ m Array.// [(i, v)]
    writeMem (MAIndirect i) l v = let i' = readMem (MADirect i) l in writeMem (MADirect i') l v
-}

------------------------------------------------------------------------
-- e. Stack 
-------------------------------------------------------------------------

newtype StackMem v = StackImpl [v]

instance Empty StackMem where
  empty = StackImpl []

instance IsEmpty StackMem where
  isEmpty (StackImpl s) = null s

instance Stack StackMem where
  push x (StackImpl s) = StackImpl (x:s)
  top (StackImpl s) = head s
  pop (StackImpl []) = error "popping of an empty stack"
  pop (StackImpl (_:ss)) = StackImpl ss
  initS = StackImpl

instance Copointed StackMem where
  copoint = top

instance Forward (StackMem v) where
  next = pop

instance  Store StackMem where
  store = push
------------------------------------------------------------------------
-- f. Tape 
-------------------------------------------------------------------------

data TMDirection = TMLeft | TMRight

newtype TapeMem v = TapeImpl ([v], v, [v])

instance Initialized TapeMem where
  inited = TapeImpl (repeat pt, pt, repeat pt)

instance FixedTape TapeMem where
  -- initT p l = TapeImpl (l, p, l)
  taperead (TapeImpl (_, p, _)) = p
  tapewrite (TapeImpl (l, _, r)) v = TapeImpl (l, v, r)

instance MoveTape TapeMem TMDirection where
  tapemove TMLeft = previous
  tapemove TMRight = next

instance Forward (TapeMem v) where
  next (TapeImpl (l, v, (x:xs))) = TapeImpl(v:l, x, xs)
  next (TapeImpl (_, _, [])) = error "non-infinite tape encountered"

instance Backward (TapeMem v) where
  previous (TapeImpl ((x:xs), v, l)) = TapeImpl (xs, x, (v:l))
  previous (TapeImpl ([], _, _)) = error "non-infinite tape encountered"

------------------------------------------------------------------------
-- g. 2 Tape Structure
-------------------------------------------------------------------------
data TMDirection2 = TLeft | TRight | TUp | TDown

-- same as the v contain 2 values
newtype  TapeMem2 v = TapeImpl2 ([TapeMem v], TapeMem v, [TapeMem v])

instance Initialized TapeMem2 where
  inited = TapeImpl2 (repeat inited, inited, repeat inited)

instance FixedTape TapeMem2 where
  taperead (TapeImpl2 (_, TapeImpl (_, p ,_), _)) = p
  tapewrite (TapeImpl2 (l, TapeImpl (pl, _, pr), r)) v = TapeImpl2 (l, TapeImpl (pl, v, pr), r)

instance MoveTape TapeMem2 TMDirection2 where
  tapemove TLeft = previous
  tapemove TRight = next
  tapemove TUp = up
  tapemove TDown = down

instance Forward (TapeMem2 v) where
  next (TapeImpl2 (ll, TapeImpl (l, v, (x:xs)), rr)) = TapeImpl2 (ll, TapeImpl(v:l, x, xs), rr)
  next (TapeImpl2 (_, TapeImpl (_, _, []), _)) = error "non-infinite tape encountered"

instance Backward (TapeMem2 v) where
  previous (TapeImpl2 (ll, TapeImpl ((x:xs), v, l), rr)) = TapeImpl2 (ll, TapeImpl (xs, x, (v:l)), rr)
  previous (TapeImpl2 (_, TapeImpl ([], _, _), _)) = error "non-infinite tape encountered"

instance Up (TapeMem2 v) where
  up (TapeImpl2 ((x:xs), v, l)) = TapeImpl2 (xs, x, (v:l))
  up (TapeImpl2 ([], _, _)) = error "non-infinite tape encountered"

instance Down (TapeMem2 v) where
  down (TapeImpl2 (l, v, (x:xs))) = TapeImpl2 (v:l, x, xs)
  down (TapeImpl2 (_, _, [])) = error "non-infinite tape encountered"

--- For now
data IOne = IOne deriving (Eq, Ord, Bounded, Enum)
data ITwo = ITwo0 | ITwo1 deriving (Eq, Ord, Bounded, Enum)
