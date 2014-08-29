{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds, Rank2Types #-}
{-# OPTIONS -Wall #-}

module RegisterMachine.Operations (
    decrReg, incrReg, incrMem, incrACC, decrMem, decrACC, halt,
    copy, copyMem, copyM2A, copyA2M, 
    lda, ldaMem, ldaACC, clearACC, clearMem,
    radd, raddMem, rsub, rsubMem, rmul, rmulMem, rdiv, rdivMem,
    choose, cmp0, cmpe0, cmpne0, jumpe0, jumpn0, jumpe0Mem, jumpe0ACC,
    cmp2, cmpe2, jumpe2, jumpe2Mem, jumpne2Mem, jumpe2AccMem, jjump, jumpImm,
    stateful_If,
    stepQ, previousQ
	) where

import Basic.Types
-- import Basic.Operations
import Basic.Features

import Control.Lens ((^.), (%~), set, Simple, Lens, Getter)

---------------------------------------------------------
------ Generic routines

-- generic get/save cycle
modifyVal :: RWValue loc mem a => (a -> a) -> loc -> mem a -> mem a
modifyVal f r m = writeMem r m (f (readMem r m))

---------------------------------------------------------
-- generic stepper, i.e. moves Q to next state
stepQ :: (HasQ st, Forward (Q st)) => st -> st
stepQ = q %~ next

-- decrease by one
previousQ :: (HasQ st, Backward (Q st)) => st -> st
previousQ = q %~ previous

-- Combine modify and step
modifyAndStep :: (HasQ st, Forward (Q st)) =>
    Simple Lens st a -> (a -> a) -> st-> st
modifyAndStep l g = stepQ . (l %~ g)

---------------------------------------------------------
------b. DR Operation
---------------------------------------------------------
-------------------Part1 : Modified version
-- THe partial recursive functions: building 'convenience instructions' using recursion
-- Beginning instruction set : {DEC, INC, JZ, H}

-- 1.1 DEC
-- 1.2 INC
decrReg :: (RWValue loc mem a, Decr a) => loc -> mem a -> mem a
decrReg = modifyVal decr

incrReg :: (RWValue loc mem a, Incr a) => loc -> mem a -> mem a
incrReg = modifyVal incr


incrMem ::  (HasQ st, Forward (Q st), HasHeap st,
    RWValue loc (Heap st) (HContents st), Incr (HContents st)) =>
    loc -> st -> st
incrMem r cm = stepQ $ heap %~ (incrReg r) $ cm

incrACC ::  (HasQ st, Forward (Q st), HasACC st,
    RWValue IMM (ACC st) (AContents st), 
    Incr (AContents st)) => st -> st
incrACC cm = modifyAndStep acc (incrReg IMM) cm

decrMem ::  (HasQ st, Forward (Q st), HasHeap st,
    RWValue loc (Heap st) (HContents st), Decr (HContents st)) => loc -> st -> st
decrMem r cm = stepQ $ heap %~ (decrReg r) $ cm

decrACC ::  (HasQ st, Forward (Q st), HasACC st,
    RWValue IMM (ACC st) (AContents st), Decr (AContents st)) => st -> st
decrACC cm = modifyAndStep acc (decrReg IMM) cm

-- 1.4 Halt
halt :: cm -> cm
halt = id

-- Inductive Function

-- 1.6 Copy
copy :: (HasQ st, Forward (Q st), SaveValue loc' f2 a, GetValue loc f1 a) =>
    Simple Lens st (f1 a) -> loc -> Simple Lens st (f2 a) -> loc' -> st -> st
copy f1 r1 f2 r2 cm =
    modifyAndStep f2 (\x -> writeMem r2 x (readMem r1 $ cm^.f1)) cm

copyMem :: (HasQ st, HasHeap st, Forward (Q st),
    GetValue loc (Heap st) (HContents st),
    SaveValue loc' (Heap st) (HContents st)) =>
    loc -> loc' -> st -> st
copyMem r1 r2 cm = copy heap r1 heap r2 cm

copyM2A :: (HasQ st, HasHeap st, Forward (Q st),
    HasACC st, AContents st ~ HContents st,
    SaveValue IMM (ACC st) (HContents st),
    GetValue loc (Heap st) (HContents st)) =>
    loc -> st -> st
copyM2A r1 cm = copy heap r1 acc IMM cm


copyA2M :: (HasQ st, Forward (Q st),
    HasHeap st, HasACC st, SaveValue loc (Heap st) (HContents st), 
    GetValue IMM (ACC st) (AContents st), (HContents st) ~ (AContents st)) =>
    loc -> st -> st
copyA2M r1 cm = copy acc IMM heap r1 cm

-- 1.7 lda
-- immediate load of a constant
lda :: (SaveValue loc f a, HasQ st, Forward (Q st))   =>
    Simple Lens st (f a) -> loc -> a -> st -> st
lda f loc v = modifyAndStep f (\x -> writeMem loc x v)

ldaMem :: (SaveValue loc (Heap st) (HContents st), HasQ st, Forward (Q st),
    HasHeap st) =>
    loc -> HContents st -> st -> st
ldaMem loc v cm = lda heap loc v cm

ldaACC :: ( SaveValue IMM (ACC st) (AContents st), 
    HasQ st, Forward (Q st),
    HasACC st) =>
    (AContents st) -> st -> st
ldaACC v cm = lda acc IMM v cm

-- 1.8 CLeaR (r).  Actually means to 'zero out'.
clearACC :: (Zero (AContents st), SaveValue IMM (ACC st) (AContents st),
    HasQ st, Forward (Q st), HasACC st) => st -> st
clearACC cm = lda acc IMM zero cm

clearMem :: (Zero (HContents st),
    SaveValue loc (Heap st) (HContents st), HasQ st, Forward (Q st), 
    HasHeap st) =>
    loc -> st -> st
clearMem loc cm = lda heap loc zero cm

-- lift a binary function "up"
type BinOp f1 loc1 f2 loc2 f3 loc3 a st =
    (HasQ st, Forward (Q st), SaveValue loc3 f3 a,
    GetValue loc1 f1 a, GetValue loc2 f2 a) =>
    Getter st (f1 a) -> loc1 -> Getter st (f2 a) -> loc2 ->
    Simple Lens st (f3 a) -> loc3 -> st -> st

lift2 :: (HasQ st, Forward (Q st), SaveValue loc3 f3 a,
    GetValue loc1 f1 a, GetValue loc2 f2 a) => (a -> a -> a) ->
    Getter st (f1 a) -> loc1 -> Getter st (f2 a) -> loc2 ->
    Simple Lens st (f3 a) -> loc3 -> st -> st
lift2 g f1 s1 f2 s2 f3 d3 cm =
    let v1 = readMem s1 $ cm^.f1 in
    let v2 = readMem s2 $ cm^.f2 in
    let m3 = cm^.f3 in
    stepQ $ set f3 (writeMem d3 m3 $ g v1 v2) cm

-- add, sub, mul, div
radd :: Additive a => BinOp f1 loc1 f2 loc2 f3 loc3 a st
radd = lift2 add
rsub :: Subtractive a => BinOp f1 loc1 f2 loc2 f3 loc3 a st
rsub = lift2 sub
rmul :: Multiplicative a => BinOp f1 loc1 f2 loc2 f3 loc3 a st
rmul = lift2 mul
rdiv :: Integral a => BinOp f1 loc1 f2 loc2 f3 loc3 a st
rdiv = lift2 div

type OpOnMemory loc st = (RWValue loc (Heap st) (HContents st), 
    HasQ st, Forward (Q st), HasHeap st) =>
                 loc -> loc -> loc -> st -> st

raddMem :: Additive (HContents st) => OpOnMemory loc st
raddMem s1 s2 d3 cm = radd heap s1 heap s2 heap d3 cm
rsubMem :: Subtractive (HContents st) => OpOnMemory loc st
rsubMem s1 s2 d3 cm = rsub heap s1 heap s2 heap d3 cm
rmulMem :: Multiplicative (HContents st) => OpOnMemory loc st
rmulMem s1 s2 d3 cm = rmul heap s1 heap s2 heap d3 cm
rdivMem :: Integral (HContents st) => OpOnMemory loc st
rdivMem s1 s2 d3 cm = rdiv heap s1 heap s2 heap d3 cm
-------------------Part2 : Unmodified version
-- 1.3 JZ
choose :: Bool -> a -> a -> a
choose c z m = if c then z else m

-- compare with 0
cmp0 :: (Zero a, GetValue loc f a) =>
    (a -> a -> Bool) -> Simple Lens st (f a) -> loc -> st -> Bool
cmp0 fun f r cm = fun (readMem r $ cm^.f) zero

cmpe0 :: (Zero a, GetValue loc f a) =>
    Simple Lens st (f a) -> loc -> st -> Bool
cmpe0 f r cm = cmp0 (==) f r cm

cmpne0 :: (Zero a, GetValue loc f a) =>
    Simple Lens st (f a) -> loc -> st -> Bool
cmpne0 f r cm = not (cmpe0 f r cm)

jumpe0 :: (Zero a,
    HasQ st, Forward (Q st),
    GetValue loc f a) =>
    Simple Lens st (f a) -> loc -> Q st -> st -> st
jumpe0 f r z cm = set q (choose (cmpe0 f r cm) z (next $ cm^.q)) cm

jumpn0 :: (Zero a,
    HasQ st, Forward (Q st),
    GetValue loc f a) =>
    Simple Lens st (f a) -> loc -> Q st -> st -> st
jumpn0 f r z cm = set q (choose (cmpne0 f r cm) z (next $ cm^.q)) cm

jumpe0Mem :: (Zero (HContents st), HasQ st,
    Forward (Q st), 
    GetValue loc (Heap st) (HContents st),
    HasHeap st) =>
    loc -> Q st -> st -> st
jumpe0Mem r z cm = jumpe0 heap r z cm

jumpe0ACC :: (Zero (AContents st), 
    HasQ st, Forward (Q st),
    GetValue IMM (ACC st) (AContents st),
    HasACC st) =>
    Q st -> st -> st
jumpe0ACC z cm = jumpe0 acc IMM z cm

-- 1.4  JE compare with two registers
cmp2 :: (Eq a, GetValue loc' f2 a, GetValue loc f1 a) =>
  (a -> a -> Bool) -> Simple Lens st (f1 a) -> loc -> Simple Lens st (f2 a) -> loc' -> st -> Bool
cmp2 fun f1 r1 f2 r2 cm = fun (readMem r1 $ cm^.f1) (readMem r2 $ cm^.f2)

cmpe2 :: (Eq a, GetValue loc' f2 a,
  GetValue loc f1 a) =>
  Simple Lens st (f1 a) -> loc -> Simple Lens st (f2 a) -> loc' -> st -> Bool
cmpe2 f1 r1 f2 r2 cm = cmp2 (==) f1 r1 f2 r2 cm
-- all the jumps below never read the Q !
jumpe2 :: (Eq a, GetValue loc' f2 a, GetValue loc f1 a,
    HasQ st 
    ) =>
    Simple Lens st (f1 a) -> loc -> Simple Lens st (f2 a) -> loc' -> Q st -> Q st -> st -> st
jumpe2 f1 r1 f2 r2 z1 z2 cm = set q (choose (cmp2 (==) f1 r1 f2 r2 cm) z1 z2) cm

jumpe2Mem :: (Eq (HContents st), 
    GetValue loc' (Heap st) (HContents st), GetValue loc (Heap st) (HContents st),
    Forward (Q st),
    HasHeap st, HasQ st) =>
    loc -> loc' -> Q st -> st -> st
jumpe2Mem r1 r2 z cm = 
    let z2 = next (cm^.q) in
    jumpe2 heap r1 heap r2 z z2 cm

jumpne2Mem :: (Eq (HContents st),
    Forward (Q st),
    GetValue loc (Heap st) (HContents st),
    HasHeap st, HasQ st) =>
    loc -> loc -> Q st -> st -> st
jumpne2Mem r1 r2 z cm = 
    let z2 = next (cm^.q) in
    jumpe2 heap r1 heap r2 z2 z cm

jumpe2AccMem :: (Eq (AContents st), HContents st ~ AContents st,
    HasQ st, Forward (Q st), GetValue loc (Heap st) (HContents st),
    GetValue IMM (ACC st) (AContents st),
    HasACC st, HasHeap st) =>
    loc -> Q st -> st -> st
jumpe2AccMem r1 z cm = 
    let z2 = next (cm^.q) in
    jumpe2 acc IMM heap r1 z z2 cm

-- 1.5 Unconditional Jump
jjump :: HasQ st => Q st -> st -> st
jjump = set q

-- 1.6 Immediate jump (too special to single-location heap?)
jumpImm :: (HasHeap st, GetValue IMM (Heap st) (HContents st),
    Forward (Q st),
    HasQ st,
    Eq (HContents st)) =>
    HContents st -> Q st -> st -> st
jumpImm v z cm =
    let v' = readMem IMM $ cm^.heap in
    let z2 = next (cm^.q) in
    set q (choose (v == v') z z2) cm

--------------------------------------------------------------------------
------------c. Extra Operation
--------------------------------------------------------------------------
stateful_If :: (t -> Bool) -> (t -> t) -> (t -> t) -> t -> t
stateful_If cond tb eb = \x -> if cond x then tb x else eb x
