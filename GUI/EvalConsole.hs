module GUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Cont
import Control.Arrow
import Control.Applicative((<$>))

import Control.Concurrent
import Control.Concurrent.STM

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,isJust)
import Data.Reference
import Data.Monoid (mempty)

import Text.Parsec
import Text.Parsec.String

import GUI.GState
import GUI.EditBook
import GUI.Config
import GUI.SymbolList
import GUI.Utils
import GUI.Console
import GUI.EvalConsole.Parser
import GUI.EvalConsole.EvalComm

import Fun.Environment
-- import Fun.Eval.Proof
import Fun.Eval.Eval


import qualified Equ.PreExpr as PE

prependPrompt = ("fun> "++)

type EvResult = Either String String

resetEnv :: GuiMonad ()
resetEnv = ask >>= \content ->
           get >>= \ref ->
           io $ do
             let entry = content ^. (gFunCommConsole . commEntry)
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
                        do _ <- entry `on` entryActivate $ io $ do                    
                               cmdLine <- entryGetText entry
                               entrySetText entry ""
                               textBufferInsertLn buf (prependPrompt cmdLine ++ "\n")
                               res <- processCmd cmdLine ref
                               putResult res buf tv
                               titer2 <- textBufferGetEndIter buf
                           -- textViewScrollToIter no anda bien, por eso uso scrollToMark
                               mark <- textBufferCreateMark buf Nothing titer2 False
                               textViewScrollToMark tv mark 0 Nothing
                               widgetShowAll tv
                           return ()

putResult :: EvResult -> TextBuffer -> TextView -> IO ()
putResult (Left er) b tv = printErrorMsg er b tv
putResult (Right e) b tv = printInfoMsg e b tv


processCmd :: String -> GStateRef -> IO EvResult
processCmd s ref = either (return . Left . show)
                          pcmd
                          (parseFromString s)
    where pcmd c = 
            readRef ref >>= \st ->
            let eExp = st ^. (gFunEvalSt . evalExpr)
                eEnv = st ^. (gFunEvalSt . evalEnv) in
            case c of
                 Load e -> newEvalEnv ref >>= \evEnv ->
                           writeRef ref 
                            ((<~) gFunEvalSt (FunEvalState (Just e) evEnv) st) >>
                           return (Right "Expresión cargada")
                 Eval e -> newEvalEnv ref >>= \evEnv ->
                           writeRef ref
                            ((<~) gFunEvalSt (FunEvalState (Just e) evEnv) st) >>
                           return (eval evEnv e) >>=
                           (return . Right . PE.prettyShow)
                 Step -> case eExp of
                              Nothing -> return $ Left "No hay expresión cargada"
                              Just eExp' ->
                                        putStrLn ("Expresión a evaluar =" ++ (show eExp')) >>
                                        putStrLn ("Env  =" ++ (show eEnv)) >>
                                        return (runStateT (evalStep eExp') eEnv) >>= \res ->
                                        putStrLn ("Resultado =" ++ (show res)) >>
                                        maybe (return $ Right $ PE.prettyShow eExp')
                                              (\(evalE,newEnv) -> 
                                                putStrLn "por escribir referencia" >>
                                                writeRef ref
                                                 ((<~) gFunEvalSt 
                                                   (FunEvalState (Just evalE) newEnv) st) >>
                                                putStrLn "referencia escrita" >>
                                                putStrLn ("Retorno = " ++ PE.prettyShow evalE) >>
                                                return (Right $ PE.prettyShow evalE))
                                              res
                                        
    
-- runCmd :: TMVar String -> TMVar EvResult -> GStateRef -> IO ()
-- runCmd chan ochan r = (getCmd >>= \cmdLine ->
--                        (^. gFunEnv) <$> readRef r >>= \env ->
--                        evaluate (parserCmdCont getCmd putRes (cfg env) cmdLine) env) >>
--                       runCmd chan ochan r
--     where putRes = atomically . putTMVar ochan
--           getCmd = atomically $ takeTMVar chan
--           evaluate cmd env = runStateT (runContT cmd return) (cfg env,mempty)
--           cfg = initConfig


