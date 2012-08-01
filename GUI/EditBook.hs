module GUI.EditBook where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Arrow

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,fromMaybe)

import GUI.GState
import GUI.Config


configTextView :: TextBufferClass buffer => Label -> buffer -> GuiMonad ()
configTextView linesI buf = liftIO $ do
        onBufferChanged buf ( do
            countlLine <- textBufferGetLineCount buf
            let iplusone = unlines [show i | i <- [1..countlLine]]
            labelSetText linesI iplusone )
        return ()
        
configLanguage :: SourceBuffer -> GuiMonad ()
configLanguage buf = liftIO $ do
        
        -- Language Spec
        slm <- sourceLanguageManagerNew
        path <- sourceLanguageManagerGetSearchPath slm
        sourceLanguageManagerSetSearchPath slm (Just $ languageSpecFolder:path)
        
        mlang <- sourceLanguageManagerGuessLanguage 
                    --slm (Just languageSpecFunFile) (Just funMimeType)
                    slm (Just languageSpecFunFile) (Just funMimeType)
        case mlang of
             Nothing -> putStrLn "WARNING: No se puede cargar el highlighting para el lenguaje"
             Just lang -> do
                    langId <- sourceLanguageGetId lang
                    putStrLn ("Lenguaje = "++show langId)
                    sourceBufferSetLanguage buf (Just lang)

                    sourceBufferSetHighlightSyntax buf True
                    sourceBufferSetHighlightMatchingBrackets buf True        
                    -- Style Scheme
                    stm <- sourceStyleSchemeManagerNew
                    sourceStyleSchemeManagerSetSearchPath stm (Just [textStylesFolder])
                    styleSch <- sourceStyleSchemeManagerGetScheme stm "fun"        
                    sourceBufferSetStyleScheme buf (Just styleSch)

configSourceView :: SourceView -> GuiMonad ()
configSourceView sv = io $ do
        sourceViewSetIndentWidth sv funIdentWidth
        sourceViewSetAutoIndent sv autoIdent
        sourceViewSetIndentOnTab sv setIndentOnTab
        sourceViewSetInsertSpacesInsteadOfTabs sv spacesInsteadTab
        

configInfoLines :: Label -> GuiMonad ()
configInfoLines l = io $
            set l [ miscXalign := 0
                  , miscYalign := 0
                  , miscXpad   := 2
                  ]
    
configScrolledWindow :: ScrolledWindow -> GuiMonad ()
configScrolledWindow sw = io $
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAutomatic 
                   ]
                   
configNotebook :: Notebook -> GuiMonad ()
configNotebook nb = io $
            set nb [ notebookTabBorder  := 0
                   , notebookTabHborder := 0
                   , notebookTabVborder := 0
                   ]

createTextEntry :: Maybe String -> GuiMonad HBox
createTextEntry mcode = do
            hbox <- io $ hBoxNew False 0
            
            l <- io $ labelNew $ Just "1"
            configInfoLines l
            
            buf <- io $ sourceBufferNew Nothing
            
            configTextView l buf
            configLanguage buf
            
            maybe (return ()) (io . loadCode buf) mcode
            
            sourceview <- io $ sourceViewNewWithBuffer buf
            
            configSourceView sourceview
            
            io $ boxPackStart hbox l PackNatural 0
            io $ boxPackStart hbox sourceview PackGrow 0
            
            return hbox
    where
        loadCode :: TextBufferClass tbuffer => tbuffer -> String -> IO ()
        loadCode buf code = do
                start <- textBufferGetStartIter buf
                textBufferInsert buf start code

createTextEdit :: Maybe String -> GuiMonad ScrolledWindow
createTextEdit mcode = do
            swindow <- io $ scrolledWindowNew Nothing Nothing
            configScrolledWindow swindow
            
            hAdj <- io $ adjustmentNew 0 0 100 1 10 10
            vAdj <- io $ adjustmentNew 0 0 100 1 10 10
            
            texte <- createTextEntry mcode
            
            portv <- io $ viewportNew hAdj vAdj
            io $ set portv   [containerChild := texte]
            
            io $ widgetShowAll texte
            
            io $ widgetShowAll portv
            
            io $ set swindow [containerChild := portv]
            
            io $ widgetShowAll swindow
            
            return swindow

getTextEditFromFunEditBook :: FunEditBook -> GuiMonad (String,TextView)
getTextEditFromFunEditBook feditBook = do
            let notebook = feditBook ^. book
            cPageNum       <- io $ notebookGetCurrentPage notebook
            Just cpSW      <- io $ notebookGetNthPage notebook cPageNum
            Just textViewN <- io $ notebookGetTabLabelText notebook cpSW
            [cpPV]    <- io $  containerGetChildren (castToContainer cpSW)
            [cpBox]   <- io $ containerGetChildren (castToContainer cpPV)
            [_,tv]    <- io $ containerGetChildren (castToContainer cpBox)
            return (textViewN,castToTextView tv)

createEditBook :: Maybe String -> Maybe String -> GuiMonad Notebook
createEditBook mname mcode = do
            let name = fromMaybe "blank" mname
            
            newnt <- io notebookNew
            configNotebook newnt
            
            texte <- createTextEdit mcode
            
            io $ notebookAppendPage newnt texte name
            
            content <- ask 
            
            let mainPaned = content ^. (gFunMainPaned . mpaned)
            
            io $ panedAdd2 mainPaned newnt
            io $ widgetShowAll mainPaned
            
            return newnt
