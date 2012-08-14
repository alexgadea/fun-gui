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

import GUI.File
import GUI.GState
import GUI.EditBook
import GUI.File
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
                        forkIO $ runCmd chan repChan
                        do _ <- entry `on` entryActivate $ io $ do                    
                               cmdLine <- entryGetText entry
                               entrySetText entry ""
                               st <- readRef ref
                               let env = st ^. gFunEnv
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

runCmd :: TMVar String -> TMVar EvResult -> IO ()
runCmd chan ochan = (getCmd >>= \cmdLine ->
                     case parserCmd cmdLine of 
                       Left err -> putRes $ errorInParsing ("Error en el comando: " ++ err)
                       Right cmd -> runStateT (runContT (evaluate cmd []) return) (cfg [],mempty) >>
                                   return ()) >> 
                     runCmd chan ochan
    where putRes str = atomically (putTMVar ochan str)
          getCmd = atomically $ takeTMVar chan
          evaluate cmd env = eval getCmd putRes cmd (return (cfg env))
          cfg = initConfig

