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

import Control.Concurrent(forkIO)

import Fun.Environment
-- import Fun.Eval.Proof
import Fun.Eval.Eval


import qualified Equ.PreExpr as PE

prependPrompt = ("fun> "++)

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
--                         forkIO (processCmd ref evIn evRes)
--                         forkIO (showResultCmd evRes buf tv mvarShow)
                        do _ <- entry `on` entryActivate $ io $
                            do
                               cmdLine <- entryGetText entry
                               forkIO (processCmd cmdLine ref >>= \res ->
                                       (putStrLn . show) res >>
                                       (postGUIAsync $ putResult res buf tv >>
                                                      scrollTV buf tv))
                               entrySetText entry ""
                               printInfoMsg (prependPrompt cmdLine ++ "\n") buf tv
                               return ()
                           return ()

putResult :: EvResult -> TextBuffer -> TextView -> IO ()
putResult (Left er) = printErrorMsg er
putResult (Right e) = printInfoMsg e

scrollTV buf tv = 
    do
        titer2 <- textBufferGetEndIter buf
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing
        widgetShowAll tv
                                
-- processCmd toma un string 
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
                                        return (runStateT (evalStep eExp') eEnv) >>= \res ->
                                        maybe (return $ Right $ PE.prettyShow eExp')
                                              (\(evalE,newEnv) -> 
                                                writeRef ref
                                                 ((<~) gFunEvalSt 
                                                   (FunEvalState (Just evalE) newEnv) st) >>
                                                return (Right $ PE.prettyShow evalE))
                                              res

