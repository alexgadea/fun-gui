module GUI.InfoConsole ( configInfoConsoleTV
                       , printInfoMsg
                       , printErrorMsg) where

import Graphics.UI.Gtk hiding (get)

import GUI.Utils
import GUI.Config
import GUI.GState
import qualified GUI.Console as Console

import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Control.Arrow

import Lens.Family


configInfoConsoleTV :: TextView -> TextBuffer -> IO ()
configInfoConsoleTV = Console.configConsoleTV 

printInfoMsg :: String -> GuiMonad ()
printInfoMsg msg = ask >>= \content ->
                   get >>= \ref ->
                   let infoBuf = content ^. (gFunInfoConsole . infoConTBuffer) in
                   let infoTV = content ^. (gFunInfoConsole . infoConTView) in
                   io $ Console.printInfoMsg msg infoBuf infoTV

printErrorMsg :: String -> GuiMonad ()
printErrorMsg msg = ask >>= \content ->
                    get >>= \ref ->
                    let infoBuf = content ^. (gFunInfoConsole . infoConTBuffer) in
                    let infoTV = content ^. (gFunInfoConsole . infoConTView) in
                    io $ Console.printErrorMsg msg infoBuf infoTV