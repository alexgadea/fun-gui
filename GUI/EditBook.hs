-- | Modulo respectivo a la parte derecha de la interfaz, es decir, el 
-- campo de texto.
module GUI.EditBook where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Arrow

import Lens.Family

import Data.Text (pack,unpack)
import Data.Maybe (fromJust,fromMaybe)
import Data.List (delete)

import GUI.GState
import GUI.Config
import GUI.Utils

import Fun.Module (ModName)

-- | Configura el label respectivo a al informe de las lineas del campo de texto.
configTextView :: TextBufferClass buffer => Label -> buffer -> GuiMonad ()
configTextView linesI buf = io $ do
        onBufferChanged buf ( do
            countlLine <- textBufferGetLineCount buf
            let iplusone = unlines [show i | i <- [1..countlLine]]
            labelSetText linesI iplusone )
        return ()

-- Configura el lenguaje para el sourceView.
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

-- | Configuración del sourceView.
configSourceView :: SourceView -> GuiMonad ()
configSourceView sv = io $ do
        sourceViewSetIndentWidth sv funIdentWidth
        sourceViewSetAutoIndent sv autoIdent
        sourceViewSetIndentOnTab sv setIndentOnTab
        sourceViewSetInsertSpacesInsteadOfTabs sv spacesInsteadTab
        
-- | Configuración del aspecto y caracteristicas del label de lineas.
configInfoLines :: Label -> GuiMonad ()
configInfoLines l = io $
            set l [ miscXalign := 0
                  , miscYalign := 0
                  , miscXpad   := 2
                  ]

-- | Configuración de la ventana de scroll, que contiene el campo de texto.
configScrolledWindow :: ScrolledWindow -> GuiMonad ()
configScrolledWindow sw = io $
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAutomatic 
                   ]

-- | Configuración del aspecto del notebook que contiene los archivos abiertos.
configNotebook :: Notebook -> GuiMonad ()
configNotebook nb = io $
            set nb [ notebookTabBorder  := 0
                   , notebookTabHborder := 0
                   , notebookTabVborder := 0
                   ]

-- | Crea un campo de texto y lo llena, de ser posible, con el string.
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

-- | Crea un campo de texto con su respectivo scrollWindow.
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

-- | Dado un EditBook, obtiene el campo de texto que esta seleccionado en
-- ese momento y su nombre.
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
            
-- | Dado un EditBook y un nombre de módulo, actualiza el mapeo entre tabs-nombres de modulos
--   asignando al tab seleccionado el nombre de modulo.
updateModulesFunEditBook :: FunEditBook -> ModName -> GuiMonad ()
updateModulesFunEditBook feditBook mName = do
    let notebook = feditBook ^. book
    let infoMods = feditBook ^. modules    
    cPageNum <- io $ notebookGetCurrentPage notebook
    
    -- actualizamos el label del tab con el nombre del modulo
    Just cpSW <- io $ notebookGetNthPage notebook cPageNum
    io $ notebookSetTabLabelText notebook cpSW (unpack mName)
    
    case lookup cPageNum infoMods of
        Nothing -> updateGState ((^=) gFunEditBook (Just $ 
                                                      feditBook { _book = notebook
                                                                , _modules = (cPageNum,mName):infoMods
                                                                } ))
        Just m -> let infoMods' = delete (cPageNum,m) infoMods in
                      updateGState ((^=) gFunEditBook (Just $ 
                                            feditBook { _book = notebook
                                                      , _modules = (cPageNum,mName):infoMods'
                                                                } ))


-- | Crea un editBook, el cual tiene un primer campo de texto con nombre 
-- y contenido de ser posible.
createEditBook :: Maybe String -> Maybe String -> GuiMonad Notebook
createEditBook mname mcode = do
            let name = fromMaybe "blank" mname
            
            newnt <- io notebookNew
            configNotebook newnt
            
            texte <- createTextEdit mcode
            
            io $ notebookAppendPage newnt texte name
            
            content <- ask 
            
            let editorPaned = content ^. (gFunEditorPaned . epaned)
            
            io $ panedAdd1 editorPaned newnt
            io $ widgetShowAll editorPaned
            
            return newnt
