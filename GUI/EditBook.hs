-- | Modulo respectivo a la parte derecha de la interfaz, es decir, el 
-- campo de texto.
module GUI.EditBook where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Control.Arrow

import Lens.Family hiding (set)

import Data.Text (pack,unpack)
import Data.Maybe (fromJust,fromMaybe)
import Data.List (delete)

import GUI.GState
import GUI.Config
import GUI.Utils

import Fun.Module (ModName)
import Fun.Parser

-- Configura el lenguaje para el sourceView.
configLanguage :: SourceBuffer -> GuiMonad ()
configLanguage buf = io $ do
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
        sourceViewSetShowLineNumbers sv True
        

-- | Configuración de la ventana de scroll, que contiene el campo de texto.
configScrolledWindow :: ScrolledWindow -> GuiMonad ()
configScrolledWindow sw = io $
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAlways
                   ]

-- | Configuración del aspecto del notebook que contiene los archivos abiertos.
configNotebook :: Notebook -> GuiMonad ()
configNotebook nb = io $
            set nb [ notebookTabBorder  := 0
                   , notebookTabHborder := 0
                   , notebookTabVborder := 0
                   ]

-- | Crea un campo de texto y lo llena, de ser posible, con el string.
createTextEntry :: Maybe String -> GuiMonad SourceView
createTextEntry mcode = do
            hbox <- io $ hBoxNew False 0
            buf <- io $ sourceBufferNew Nothing
            configLanguage buf
            
            maybe (return ()) (io . loadCode buf) mcode
            
            sourceview <- io $ sourceViewNewWithBuffer buf

            configSourceView sourceview
            
            return sourceview
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
                        
            texte <- createTextEntry mcode
            
            io $  containerAdd swindow texte
            
            io $ widgetShowAll texte
            
            io $ widgetShowAll swindow
            
            return swindow

getTextViewFilePath :: Int -> GuiMonad (Maybe TextFilePath)
getTextViewFilePath i = do
                st <- getGState
                let feditBook = st ^. gFunEditBook 
                maybe (return Nothing) (return . takeFilePath) feditBook
    where
        takeFilePath :: FunEditBook -> Maybe TextFilePath
        takeFilePath feb = (feb ^. tabFileList) !! i

-- | Dado un EditBook, obtiene el campo de texto que esta seleccionado en
-- ese momento y su nombre.
getTextEditFromFunEditBook :: FunEditBook -> 
                              GuiMonad (Maybe TextFilePath, String,TextView)
getTextEditFromFunEditBook feditBook = do
            let notebook = feditBook ^. book
            cPageNum       <- io $ notebookGetCurrentPage notebook
            mfp            <- getTextViewFilePath cPageNum
            Just cpSW      <- io $ notebookGetNthPage notebook cPageNum
            Just textViewN <- io $ notebookGetTabLabelText notebook cpSW
            [tv]           <- io $ containerGetChildren (castToContainer cpSW)
            return (mfp,textViewN,castToTextView tv)
            
-- | similar a la anterior pero en la mónada IO. Le debemos pasar el notebook.
getTextEditFromNotebook :: Notebook -> IO (String,TextView)
getTextEditFromNotebook notebook = do
            cPageNum       <- notebookGetCurrentPage notebook
            Just cpSW      <- notebookGetNthPage notebook cPageNum
            Just textViewN <- notebookGetTabLabelText notebook cpSW
            [tv]           <- containerGetChildren (castToContainer cpSW)
            return (textViewN,castToTextView tv)
            
-- | Dado un EditBook y un nombre de módulo, actualiza el mapeo entre tabs-nombres de modulos
--   asignando al tab seleccionado el nombre de modulo.
updateModulesFunEditBook :: FunEditBook -> ModName -> GuiMonad ()
updateModulesFunEditBook feditBook mName = do
    let notebook = feditBook ^. book  
    cPageNum <- io $ notebookGetCurrentPage notebook
    
    -- actualizamos el label del tab con el nombre del modulo
    Just cpSW <- io $ notebookGetNthPage notebook cPageNum
    io $ notebookSetTabLabelText notebook cpSW (unpack mName)


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
