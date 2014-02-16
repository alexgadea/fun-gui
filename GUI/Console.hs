module GUI.Console (configConsoleTV,printInfoMsg, printErrorMsg) where

import Graphics.UI.Gtk hiding (get)

import GUI.Utils
import GUI.Config

configConsoleTV :: TextView -> TextBuffer -> IO ()
configConsoleTV tv buf = do
        -- Tags para el text buffer, para formatear texto:
        tagTable <- textBufferGetTagTable buf
        tag <- textTagNew (Just "ErrorScheme")
        set tag [ textTagForegroundGdk := textErrColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag
        
        tag' <- textTagNew (Just "InfoScheme")
        set tag' [ textTagForegroundGdk := textColorCommTV
                , textTagForegroundSet := True]
        textTagTableAdd tagTable tag'
        
        widgetModifyBase tv StateNormal backColorCommTV
        widgetModifyText tv StateNormal textColorCommTV
        widgetShowAll tv        

printInfoMsg :: String -> TextBuffer -> TextView -> IO ()
printInfoMsg msg = printMsg msg "InfoScheme"
                
printErrorMsg :: String -> TextBuffer -> TextView -> IO ()
printErrorMsg msg = printMsg msg "ErrorScheme"

printMsg :: String -> TagName -> TextBuffer -> TextView -> IO ()
printMsg msg tagname infoBuf infoTV  =
    io $ do titer <- textBufferGetEndIter infoBuf
            lineStart <- textIterGetLine titer
                
            -- Ingresamos el texto en el buffer
            putStrAtEnd infoBuf infoTV msg
                
            titer' <- textBufferGetEndIter infoBuf
            lineEnd <- textIterGetLine titer'
                
            start <- textBufferGetIterAtLine infoBuf lineStart
            end <- textBufferGetIterAtLine infoBuf lineEnd
                
            textBufferApplyTagByName infoBuf tagname start end
            widgetShowAll infoTV
        
        