module GUI.InfoConsole where

import Graphics.UI.Gtk hiding (get)

import GUI.Utils
import GUI.Config
import GUI.GState

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow

import Lens.Family


configInfoConsoleTV :: TextView -> TextBuffer -> IO ()
configInfoConsoleTV tv buf = do
        -- Tags para el text buffer, para formatear texto:
        tagTable <- textBufferGetTagTable buf
        tag <- textTagNew (Just "RedColor")
        set tag [textTagForeground := "red", textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        tag <- textTagNew (Just "WhiteColor")
        set tag [textTagForeground := "white", textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        widgetModifyBase tv StateNormal backColorCommTV
        widgetModifyText tv StateNormal textColorCommTV
        widgetShowAll tv
        
        
        
printInfoMsg :: String -> GuiMonad ()
printInfoMsg msg =
    printMsg msg "WhiteColor"
                
printErrorMsg :: String -> GuiMonad ()
printErrorMsg msg =
    printMsg msg "RedColor"

printMsg :: String -> TagName -> GuiMonad ()
printMsg msg tagname =
    ask >>= \content ->
    get >>= \ref ->
    let infoBuf = content ^. (gFunInfoConsole . infoConTBuffer) in
        let infoTV = content ^. (gFunInfoConsole . infoConTView) in
            io $ do
                titer <- textBufferGetEndIter infoBuf
                lineStart <- textIterGetLine titer
                
                -- Ingresamos el texto en el buffer
                putStrAtEnd infoBuf infoTV msg
                
                titer <- textBufferGetEndIter infoBuf
                lineEnd <- textIterGetLine titer
                
                start <- textBufferGetIterAtLine infoBuf lineStart
                end <- textBufferGetIterAtLine infoBuf lineEnd
                
                textBufferApplyTagByName infoBuf tagname start end
                widgetShowAll infoTV
        