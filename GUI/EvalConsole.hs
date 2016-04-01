{-# LANGUAGE FlexibleContexts #-}
module GUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)

import Control.Lens
import Control.Concurrent
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State (runStateT)

import Data.Reference
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Strict.Either as SEither

import GUI.GState hiding (eval)
import GUI.Utils
import GUI.Console
import GUI.EvalConsole.Parser
import GUI.EvalConsole.EvalComm

import Equ.PreExpr

import Fun.Environment
import Fun.Eval.Eval


import qualified Equ.PreExpr as PE

prependPrompt :: String -> String
prependPrompt = ("fun> "++)

resetEnv :: GuiMonad ()
resetEnv = ask >>= \content ->
           io $ do
             let buf = content ^. (gFunCommConsole . commTBuffer)
             let tv = content ^. (gFunCommConsole . commTView)
             printInfoMsg "Modulo cargado" buf tv


configCommandConsole :: GuiMonad ()
configCommandConsole= ask >>= \content ->
                      get >>= \ref ->
                      io $ do
                        let entry = content ^. (gFunCommConsole . commEntry)
                        let buf = content ^. (gFunCommConsole . commTBuffer)
                        let tv = content ^. (gFunCommConsole . commTView)
                        configConsoleTV tv buf
--                         forkIO (processCmd ref evIn evRes)
--                         forkIO (showResultCmd evRes buf tv mvarShow)
                        do _ <- entry `on` entryActivate $ io $
                            do
                               cmdLine <- entryGetText entry
                               _ <- forkIO (processCmd cmdLine ref >>= \res ->
                                       res `seq`
                                       (postGUIAsync $ putResult res buf tv >>
                                                      scrollTV buf tv))
                               entrySetText entry ""
                               printInfoMsg (prependPrompt cmdLine) buf tv
                               return ()
                           return ()

putResult :: EvResult -> TextBuffer -> TextView -> IO ()
putResult (SEither.Left er) = printErrorMsg er
putResult (SEither.Right e) = printInfoMsg e

scrollTV :: (TextViewClass self1, TextBufferClass self) =>
            self -> self1 -> IO ()
scrollTV buf tv = 
    do
        titer2 <- textBufferGetEndIter buf
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing
        widgetShowAll tv
                                
-- processCmd toma un string 
processCmd :: String -> GStateRef -> IO EvResult
processCmd s ref = either (return . SEither.Left . ("ERROR: "++) . show)
                          pcmd
                          (parseFromString s)
    where pcmd c = 
            readRef ref >>= \st ->
            let eExp = st ^. (gFunEvalSt . evalExpr)
                eEnv = st ^. (gFunEvalSt . evalEnv)
                eLComm = st ^. (gFunEvalSt . evalLComm)
                fenv = st ^. gFunEnv in
            case c of
                 Load e -> newEvalEnv ref >>= \evEnv ->
                           writeRef ref 
                            ((.~) gFunEvalSt (FunEvalState (Just e) evEnv (Just $ Load e)) st) >>
                           return (SEither.Right $ "Expresión cargada" ++ show e)
                 com@(Eval e) -> processEval com e ref st eval PE.prettyShow
                 com@Step -> processStep com eExp eEnv st evalStep 
                                    (showWithNewFuncs fenv) id
                 com@StepTrace -> processStep com eExp eEnv st evalStepTrace
                                   (\evEnv' (e,rulename) -> 
                                        (maybe "" ((flip (++) "\n") . PE.prettyShow) eExp)++
                                            "~~>\t["++rulename++"]\n"++
                                            (showWithNewFuncs fenv evEnv' e))
                                   fst
                 com@(EvalTrace e) -> processEval com e ref st evalTrace 
                                  (((++) (PE.prettyShow e)) . showTrace . snd)
                 LastComm -> maybe (return $ SEither.Right "")
                                   pcmd
                                   eLComm
                                   
          processStep comm eExp eEnv st evalF fshow fgetE = 
              case eExp of
                  Nothing -> return $ SEither.Left "No hay expresión cargada"
                  Just eExp' -> return (runStateT (evalF eExp') eEnv) >>= 
                                maybe (return $ SEither.Right $ PE.prettyShow eExp')
                                      (\(evalE,newEnv) -> writeRef ref
                                         ((.~) gFunEvalSt (FunEvalState 
                                            (Just $ fgetE evalE) newEnv (Just comm)) st) >>
                                                return (SEither.Right $ fshow newEnv evalE))
          
          processEval comm e gsref st feval fshow =
                           newEvalEnv gsref >>= \evEnv ->
                           writeRef gsref
                            ((.~) gFunEvalSt (FunEvalState (Just e) evEnv (Just comm)) st) >>
                           return (feval e evEnv) >>=
                           (return . SEither.Right . fshow)
                           
          showTrace ((rulename,e):ls) = 
              "\n~~>\t["++rulename++"]\n" ++ (PE.prettyShow e) ++ showTrace ls
          showTrace [] = "\n"
              

-- | Esta función se define para mostrar las expresiones
--   en una evaluación, en la cual se han definido funciones auxiliares.
--   (gralmente sucede cuando una función tiene más de 1 argumento, y entonces
--   para evaluar se definen funciones auxiliares con menos argumentos).
--   Una expresión resultante de una evaluación como esta, se mostrará junto
--   con la definición de esas variables auxiliares.
showWithNewFuncs :: Environment -> EvalEnv -> PreExpr -> String
showWithNewFuncs env evEnv e =
    let origEnv = createEvalEnv (getFuncs env) (getVals env) in
        let difEnv = M.difference evEnv origEnv in
            if not (M.null difEnv)
               then PE.prettyShow e ++ (showVars difEnv)
               else PE.prettyShow e
               
    where showVars denv = "\n\n{ " ++ (concatMap showVarDef $ M.toList $ delVars denv e) ++
                          "}\n"
          showVarDef (v,(params,expr)) = show v ++ (showParams params) ++ 
                                      " := " ++ (PE.prettyShow expr) ++ ";\n"
          showParams = concatMap (\v -> " "++ (show v))
          -- delVars elimina de un mapa las variables que no ocurren en la expresion e
          delVars m expr = let varsInE = freeVars expr in
                            foldl (\m' (v,e') -> if S.member v varsInE
                                                   then M.insert v e' m'
                                                   else m')
                                  M.empty (M.toList m)
              
