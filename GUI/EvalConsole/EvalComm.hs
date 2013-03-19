module GUI.EvalConsole.EvalComm where

import Equ.PreExpr


data EvalComm = Load PreExpr | Step | Eval PreExpr
    deriving Show
    
    

