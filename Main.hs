{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Main(main) where

import Basic.Types (readMem, copoint)
import Basic.Operations(run, runout)
import Basic.Features (q, state, ip, HasQ, State, HasState, Q, heap, output)
import Basic.MemoryImpl(Address(A))
import Basic.Memory (top, isEmpty)
import qualified PushdownAutomaton.Models.PDA1 as PDA1
import qualified PushdownAutomaton.Models.PDA2 as PDA2
import qualified PushdownAutomaton.Models.PDA3 as PDA3
import qualified TuringMachine.Models.TM1    as TM1
import qualified TuringMachine.Models.TM2    as TM2
import qualified TuringMachine.Models.TM3    as TM3
import qualified TuringMachine.Models.TM4    as TM4
import qualified TuringMachine.Models.TM5    as TM5
import qualified FiniteStateMachine.Programs.FSM1 as FSM1
import qualified FiniteStateMachine.Programs.FSM2 as FSM2
import qualified FiniteStateMachine.Programs.FSM3 as FSM3
import qualified FiniteStateMachine.Programs.FSM4 as FSM4
import qualified FiniteStateMachine.Programs.FSM5 as FSM5
import qualified FiniteStateMachine.Programs.FSM6 as FSM6
import qualified RegisterMachine.Models.CMA1 as CMA1
import qualified RegisterMachine.Models.CMA2 as CMA2
import qualified RegisterMachine.Models.CMA3 as CMA3
import qualified RegisterMachine.Models.CMA4 as CMA4
import qualified RegisterMachine.Models.CMA5 as CMA5
import qualified RegisterMachine.Models.CMA6 as CMA6
import qualified RegisterMachine.Models.CMA7 as CMA7
import qualified RegisterMachine.Models.CMA8 as CMA8
import qualified RegisterMachine.Models.CMA9 as CMA9
import qualified RegisterMachine.Models.CMA10 as CMA10
import qualified RegisterMachine.Models.CMA11 as CMA11
import qualified RegisterMachine.Models.CMA12 as CMA12
import qualified RegisterMachine.Models.CMA13 as CMA13
import qualified RegisterMachine.Models.CM1  as CM1
import qualified RegisterMachine.Models.CM2  as CM2
import qualified RegisterMachine.Models.CM3  as CM3
import qualified RegisterMachine.Models.SCM1 as SCM1
import qualified RegisterMachine.Models.SCM2 as SCM2
import qualified RegisterMachine.Models.SCM3 as SCM3
import qualified RegisterMachine.Models.RAM1 as RAM1
import qualified RegisterMachine.Models.RAM2 as RAM2
import qualified RegisterMachine.Models.RAM3 as RAM3
import qualified LambdaCalculus.Models.SECD1 as SECD1
import qualified LambdaCalculus.Models.SECD2 as SECD2
import qualified LambdaCalculus.Models.SECD3 as SECD3
import LambdaCalculus.Types
import Control.Lens

pr :: (HasQ (State m), HasState m, Show (Q (State m))) => m -> IO ()
pr = print . view q . view state

main :: IO ()
main = 
  do
    print "PDA1 Q ::"; pr pda1
    print "PDA2 Q ::"; pr pda2
    print "PDA3 Q ::"; pr pda3
    print "PDA4 Q ::"; pr pda4
    print "PDA5 Q ::"; pr pda5
    print "PDA6 Q ::"; pr pda6

    print "TM1  Q ::"; pr tm1
    print "TM2  Q ::"; pr tm2
    print "TM3  Q ::"; pr tm3
    print "TM4  Q ::"; pr tm4
    print "TM5  Q ::"; pr tm5

    print "FSM1  Q ::"; pr fsm1
    print "FSM2  Q ::"; pr fsm2
    print "FSM3  Q ::"; pr fsm3
    print "FSM4  Q ::"; pr fsm4
    print "FSM5  Q ::"; pr fsm5
    print "FSM6  Q ::"; pr fsm6
    print "FSM7  Q ::"; pr fsm7
    print "FSM8  Q ::"; pr fsm8

    print "RM1  Q ::"; pr rm1
    print "RM2  Q ::"; pr rm2
    print "RM3  Q ::"; pr rm3
    print "RM4  Q ::"; pr rm4
    print "RM5  Q ::"; pr rm5
    print "RM6  Q ::"; pr rm6
    print "RM7  Q ::"; pr rm7
    print "RM8  Q ::"; pr rm8
    print "RM9  Q ::"; pr rm9
    print "RM10 Q ::"; pr rm10
    print "RM11 Q ::"; pr rm11
    print "RM12 Q ::"; pr rm12
    print "RM13 Q ::"; pr rm13
    print "CM1  Q ::"; pr cm1
    print "CM2  Q ::"; pr cm2
    print "CM3  Q ::"; pr cm3
    print "SCM1 Q ::"; pr scm1
    print "SCM1 Q ::"; pr scm2
    print "SCM1 Q ::"; pr scm3
    print "RAM1 Q ::"; pr ram1
    print "RAM2 Q ::"; pr ram2
    print "RAM3 Q ::"; pr ram3

    -- some tests which are model specific
    print "FSM5 output"; print $ copoint $ view output fsm5
    print "FSM6 output"; print $ copoint $ view output fsm6
    print "FSM7 output"; print $ copoint $ view output fsm7
    print "FSM8 output"; print $ copoint $ view output fsm8
    print "FSM5 output"; print $ isEmpty $ view output fsm5
    print "FSM6 output"; print $ isEmpty $ view output fsm6
    print "FSM7 output"; print $ isEmpty $ view output fsm7
    print "FSM8 output"; print $ isEmpty $ view output fsm8
    print "RM  Mem ::"; print $ readMem (A (0 :: Int)) $ view heap $ view state rm2
    print "SECD1  S ::"; print $ top $ view stck $ view state secd1
    print "SECD2  S ::"; print $ top $ view stck secd2
    print "SECD3  S ::"; print $ top $ view stck $ view state secd3
    print "PDA1 IP ::"; print $ view ip $ view state pda1
    print "PDA2 IP ::"; print $ view ip $ view state pda2
    print "PDA3 IP ::"; print $ view ip $ view state pda3

    where
      pda1 = run PDA1.mach
      pda2 = run PDA2.mach
      pda3 = run PDA3.mach
      pda4 = run PDA1.mach2
      pda5 = run PDA2.mach2
      pda6 = run PDA3.mach2

      tm1  = run TM1.mach
      tm2  = run TM2.mach
      tm3  = run TM3.mach
      tm4  = run TM4.mach
      tm5  = run TM5.mach
      
      fsm1 = run FSM1.mach
      fsm2 = run FSM2.mach
      fsm3 = run FSM3.mach
      fsm4 = run FSM4.mach
      fsm5 = runout FSM5.mach
      fsm6 = runout FSM6.mach
      fsm7 = runout FSM5.mach2
      fsm8 = runout FSM6.mach2

      rm1  = run CMA1.mach
      rm2  = run CMA2.mach
      rm3  = run CMA3.mach
      rm4  = run CMA4.mach
      rm5  = run CMA5.mach
      rm6  = run CMA6.mach
      rm7  = run CMA7.mach
      rm8  = run CMA8.mach
      rm9  = run CMA9.mach
      rm10  = run CMA10.mach
      rm11  = run CMA11.mach
      rm12  = run CMA12.mach
      rm13  = run CMA13.mach
      cm1  = run CM1.mach2
      cm2  = run CM2.mach
      cm3  = run CM3.mach
      scm1  = run SCM1.mach
      scm2  = run SCM2.mach
      scm3  = run SCM3.mach
      ram1  = run RAM1.mach
      ram2  = run RAM2.mach
      ram3  = run RAM3.mach

      secd1 = run SECD1.mach
      secd2 = run SECD2.mach
      secd3 = run SECD3.mach

