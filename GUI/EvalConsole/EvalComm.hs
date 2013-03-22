module GUI.EvalConsole.EvalComm where

import Equ.PreExpr


data EvalComm = Load PreExpr | Step | Eval PreExpr | StepTrace 
                | EvalTrace PreExpr | LastComm

instance Show EvalComm where
    show (Load e) = "load " ++ prettyShow e
    show Step = "step"
    show StepTrace = "steptrace"
    show (EvalTrace e) = "evaltrace " ++ prettyShow e
    show (Eval e) = prettyShow e
    show LastComm = ""
    
    

