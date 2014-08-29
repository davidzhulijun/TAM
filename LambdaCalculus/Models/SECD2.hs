{-# OPTIONS -Wall #-}

module LambdaCalculus.Models.SECD2 (
  prog, mach
  ) where
import Basic.Memory
import Basic.MemoryImpl
import LambdaCalculus.Types
import LambdaCalculus.Machine2
import LambdaCalculus.Operations

--------------------------------------------------------------------------
--------------------------specilized model operations & models
--------------------------------------------------------------------------
prog :: Term
prog = AppT (AbsT (Var "xx") (AppT (AbsT (Var "yy") (VarT (Var "yy"))) (VarT (Var "xx")))) (ConstT (IntegerC 3))

mach :: SECD1 StackMem (Atom MapMem) (MapMem Var (Atom MapMem)) Term
mach = SECD empty emptyMap (initS [prog]) empty trans
