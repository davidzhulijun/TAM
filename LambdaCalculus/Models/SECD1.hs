{-# OPTIONS -Wall #-}

module LambdaCalculus.Models.SECD1 (
  prog, mach
  ) where
import Basic.Memory
import Basic.MemoryImpl
import LambdaCalculus.Types
import LambdaCalculus.Machine
import LambdaCalculus.State
import LambdaCalculus.Operations

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
prog :: Term
prog = AppT (AbsT (Var "xx") (AppT (AbsT (Var "yy") (VarT (Var "yy"))) (VarT (Var "xx")))) (ConstT (IntegerC 3))

mach ::  SECD (SECDState (Atom MapMem) (MapMem Var (Atom MapMem)) StackMem Term)
mach = SECD (SECDState empty emptyMap (initS [prog]) empty) transtate
