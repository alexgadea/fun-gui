module GUI.InfoConsole ( configInfoConsoleTV
                       , printInfoMsg
                       , printErrorMsg) where

import Graphics.UI.Gtk hiding (get)

import GUI.Utils
import GUI.GState
import qualified GUI.Console as Console

import Control.Lens
import Control.Monad.Trans.RWS

configInfoConsoleTV :: TextView -> TextBuffer -> IO ()
configInfoConsoleTV = Console.configConsoleTV 

printInfoMsg :: String -> GuiMonad ()
printInfoMsg msg = ask >>= \content ->
                   let infoBuf = content ^. (gFunInfoConsole . infoConTBuffer) in
                   let infoTV = content ^. (gFunInfoConsole . infoConTView) in
                   io $ Console.printInfoMsg msg infoBuf infoTV

printErrorMsg :: String -> GuiMonad ()
printErrorMsg msg = ask >>= \content ->
                    let infoBuf = content ^. (gFunInfoConsole . infoConTBuffer) in
                    let infoTV = content ^. (gFunInfoConsole . infoConTView) in
                    io $ Console.printErrorMsg msg infoBuf infoTV