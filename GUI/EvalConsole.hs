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

import Fun.Environment
import Fun.Eval.Interact
-- import Fun.Eval.Proof
import Fun.Eval.Parser
import Fun.Eval.EvalMonad

prependPrompt = ("fun> "++)

resetEnv :: GuiMonad ()
resetEnv = ask >>= \content ->
           get >>= \ref ->
           io $ do
             let entry = content ^. (gFunCommConsole . commEntry)
             let buf = content ^. (gFunCommConsole . commTBuffer)
             let tv = content ^. (gFunCommConsole . commTView)
             let chan = content ^. (gFunCommConsole . commChan)
             let repChan = content ^. (gFunCommConsole . commRepChan)
             () <- atomically (putTMVar chan "reset")
             _ <- atomically (takeTMVar repChan)
             printInfoMsg "Modulo cargado" buf tv 


configCommandConsole :: GuiMonad ()
configCommandConsole= ask >>= \content ->
                      get >>= \ref ->
                      io $ do
                        let entry = content ^. (gFunCommConsole . commEntry)
                        let buf = content ^. (gFunCommConsole . commTBuffer)
                        let tv = content ^. (gFunCommConsole . commTView)
                        let chan = content ^. (gFunCommConsole . commChan)
                        let repChan = content ^. (gFunCommConsole . commRepChan)
                        configConsoleTV tv buf
                        forkIO $ runCmd chan repChan ref
                        do _ <- entry `on` entryActivate $ io $ do                    
                               cmdLine <- entryGetText entry
                               entrySetText entry ""
                               atomically $ putTMVar chan cmdLine
                               res <- atomically $ takeTMVar repChan
                               textBufferInsertLn buf (prependPrompt cmdLine ++ "\n")
                               putResult res buf tv
                               titer2 <- textBufferGetEndIter buf
                           -- textViewScrollToIter no anda bien, por eso uso scrollToMark
                               mark <- textBufferCreateMark buf Nothing titer2 False
                               textViewScrollToMark tv mark 0 Nothing
                               widgetShowAll tv
                           return ()

putResult :: EvResult -> TextBuffer -> TextView -> IO ()
putResult (EvErr e) = printErrorMsg (show e) 
putResult (EvOk r) = printInfoMsg r 

runCmd :: TMVar String -> TMVar EvResult -> GStateRef -> IO ()
runCmd chan ochan r = (getCmd >>= \cmdLine ->
                       (^. gFunEnv) <$> readRef r >>= \env ->
                       evaluate (parserCmdCont getCmd putRes (cfg env) cmdLine) env) >>
                      runCmd chan ochan r
    where putRes = atomically . putTMVar ochan
          getCmd = atomically $ takeTMVar chan
          evaluate cmd env = runStateT (runContT cmd return) (cfg env,mempty)
          cfg = initConfig


