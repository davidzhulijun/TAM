{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module LambdaCalculus.Operations(
  getvalfromenv, setvalinenv, transtate, trans
  ) where


import Basic.Memory
import Basic.Types (Trans(Trans))
import LambdaCalculus.Types

import Control.Lens

-- 1. get the value from the environment
getvalfromenv :: MapLike m => Var -> m Var (Atom m) -> Atom m
getvalfromenv v m = if (memberM m v)
  then lookupM m v
  else ErrorA "invalid v in the environment"

setvalinenv :: MapLike m => Var ->  Atom m -> m Var (Atom m) -> m Var (Atom m)
setvalinenv = insertM

transtate :: (Stack s, MapLike m, HasCtrl st, HasDump st, HasEnv st, HasStack st,
  s Term ~ Ctrl st, Dump st ~ s (s (Atom m), m Var (Atom m), s Term),
  Env st ~ m Var (Atom m), Stck st ~ s (Atom m)) => Trans () st
transtate = Trans f
 where
  f () st = 
    let s = st^.stck
        e = st^.env
        c = st^.ctrl
        d = st^.dump in
    case (isEmpty c) of
      True ->
              stck .~ (push (top s) s1) $
              env .~ e1 $
              ctrl .~ c1 $
              dump .~ (pop d) $ st
              where (s1, e1, c1) = top d
      False -> 
        case (top c) of
          (ConstT x) -> stck .~ (push (ConstA x) s) $ ctrl .~ (pop c) $ st
          (VarT x)   -> stck .~ (push (getvalfromenv x e) s) $ ctrl .~ (pop c) $ st
          (AppT t1 t2) -> ctrl .~ (push t1 $ push t2 $ push (PrimT ApplicationPP) $ (pop c)) $ st
          (AbsT var t) -> stck .~ (push (ClosureA var t e) s) $
                          ctrl .~ (pop c) $ st
          (PrimT ApplicationPP) -> 
            case (top sx) of 
              ClosureA var t e1 -> stck .~ empty $
                                   env .~ (insertM var (top s) e1) $
                                   ctrl .~ (initS [t]) $
                                   dump .~ (push ((pop sx), e, (pop c)) d) $ st
              _  -> error "invalid Operations"  
              where sx = pop s
          _  -> error "invalid Operations"

trans :: (Stack st, MapLike m, s ~ st (Atom m), e ~ m Var (Atom m), c ~ st Term,
  d ~ st (s, e, c))=> Trans () (s, e, c, d)
trans = Trans f
 where
  f () (s, e, c, d) = 
    case (isEmpty c) of
      True -> (s', e', c', d') where
              s' = (push (top s) s1) 
              e' = e1 
              c' = c1 
              d' = pop d
              (s1, e1, c1) = top d
      False -> 
        case (top c) of
          (ConstT x) -> (s', e, c', d) where
                         s' = (push (ConstA x) s) 
                         c' = (pop c)
          (VarT x)   -> (s', e, c', d) where
                         s' = (push (getvalfromenv x e) s) 
                         c' = (pop c)
          (AppT t1 t2) -> (s, e, c', d) where 
                         c' = (push t1 $ push t2 $ push (PrimT ApplicationPP) $ (pop c))
          (AbsT var t) -> (s', e, c', d) where
                         s' = (push (ClosureA var t e) s) 
                         c' = (pop c)
          (PrimT ApplicationPP) -> 
            case (top sx) of 
              ClosureA var t e1 -> (s', e', c', d') where
                                   s' = empty
                                   e' = (insertM var (top s) e1)
                                   c' = (initS [t]) 
                                   d' = (push ((pop sx), e, (pop c)) d)
              _  -> error "invalid Operations"  
              where sx = pop s
          _  -> error "invalid Operations"