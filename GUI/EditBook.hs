{-# Language DoAndIfThenElse #-}
-- | Modulo respectivo a la parte derecha de la interfaz, es decir, el 
-- campo de texto.
module GUI.EditBook where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Lens hiding (set)
import Control.Monad (void)
import Control.Monad.Trans.RWS

import Data.Text  (Text, unpack, pack)
import qualified Data.Text as T (concat,length)
import Data.Maybe (fromMaybe)

import GUI.GState
import GUI.Completion
import GUI.Config
import GUI.Utils

import Fun.Module (ModName)
import Fun.Parser
import Paths_fun_gui

-- Configura el lenguaje para el sourceView.
configLanguage :: SourceBuffer -> GuiMonad ()
configLanguage buf = io $ do
    -- Language Spec
    slm <- sourceLanguageManagerNew
    path <- sourceLanguageManagerGetSearchPath slm
    langSpecFolder <- getDataFileName languageSpecFolder
    sourceLanguageManagerSetSearchPath slm (Just $ langSpecFolder:path)
    
    mlang <- sourceLanguageManagerGuessLanguage 
                --slm (Just languageSpecFunFile) (Just funMimeType)
                slm (Just languageSpecFunFile) (Just funMimeType)
    case mlang of
        Nothing -> putStrLn "WARNING: No se puede cargar el highlighting para el lenguaje"
        Just lang -> do
            sourceBufferSetLanguage buf (Just lang)

            sourceBufferSetHighlightSyntax buf True
            sourceBufferSetHighlightMatchingBrackets buf True        
            -- Style Scheme
            stm <- sourceStyleSchemeManagerNew
            txtStyleFolder <- getDataFileName textStylesFolder
            sourceStyleSchemeManagerSetSearchPath stm (Just [txtStyleFolder])
            styleSch <- sourceStyleSchemeManagerGetScheme stm "fun"        
            sourceBufferSetStyleScheme buf (Just styleSch)

-- | Configuración del sourceView.
configSourceView :: SourceView -> GuiMonad ()
configSourceView sv = ask >>= \cnt -> get >>= \stref ->
   io $ do
        sourceViewSetIndentWidth sv funIdentWidth
        sourceViewSetAutoIndent sv autoIdent
        sourceViewSetIndentOnTab sv setIndentOnTab
        sourceViewSetInsertSpacesInsteadOfTabs sv spacesInsteadTab
        sourceViewSetShowLineNumbers sv True

        void $ io $ on sv keyPressEvent    $ deleteCompl cnt stref
        void $ io $ on sv moveCursor       $ stopCompl cnt stref
        void $ io $ on sv buttonPressEvent $ eventStopCompl cnt stref
        void $ io $ on sv focusOutEvent    $ eventStopCompl cnt stref

        return ()
    where
        eventStopCompl :: GReader -> GStateRef -> EventM t Bool
        eventStopCompl cnt stref = io $
                  eval (updateGState (gFunCompletion .~ Nothing)) cnt stref >>
                  return False

        stopCompl :: GReader -> GStateRef -> MovementStep -> Int -> Bool -> IO ()
        stopCompl cnt stref _ _ _ =
                  eval (updateGState (gFunCompletion .~ Nothing)) cnt stref

        deleteCompl :: GReader -> GStateRef -> EventM EKey Bool
        deleteCompl cnt stref = eventKeyName >>= \key ->
                     if key == pack "BackSpace"
                     then io (eval delCompl cnt stref >> return False)
                     else return False

        delCompl :: GuiMonad ()
        delCompl = updateGState
                   (\st -> maybe
                           st
                           (\cpl -> (.~) gFunCompletion
                                    (Just $ rmCharToCompletion cpl) st)
                           (st ^. gFunCompletion)
                   )

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
            buf <- io $ sourceBufferNew Nothing
            configLanguage buf

            maybe (return ()) (io . loadCode buf) mcode

            configSourceBuffer buf

            sourceview <- io $ sourceViewNewWithBuffer buf

            configSourceView sourceview

            return sourceview
    where
        loadCode :: TextBufferClass tbuffer => tbuffer -> String -> IO ()
        loadCode buf code = do
                start <- textBufferGetStartIter buf
                textBufferInsert buf start code

configSourceBuffer :: SourceBuffer -> GuiMonad ()
configSourceBuffer sb = ask >>= \cnt -> get >>= \stref -> io $ do
        void $ after sb bufferInsertText $ insCompl cnt stref
   where
        insCompl :: GReader -> GStateRef -> TextIter -> Text -> IO ()
        insCompl cnt stref ti str = eval (putCompletion ti str) cnt stref

        putCompletion :: TextIter -> Text -> GuiMonad ()
        putCompletion ti str
               | checkBeginChar str =
                 updateGState (gFunCompletion .~ (Just $ newCompletion))
               | checkEndChar str = tryPutSymbol ti str
               | otherwise =
                 getGState >>= \st ->
                 maybe (return ()) (putCompl str) (st ^. gFunCompletion)

        tryPutSymbol :: TextIter -> Text -> GuiMonad ()
        tryPutSymbol ti str = getGState >>= \st ->
                          maybe (updateGState (gFunCompletion .~ Nothing))
                                (replaceSymbol ti str)
                                (maybe Nothing
                                       checkCompletion (st ^. gFunCompletion))

        replaceSymbol :: TextIter -> Text -> (Text, Text) -> GuiMonad ()
        replaceSymbol eit str (sy,nname) = io (
                      textIterCopy eit >>= \bit ->
                      textIterBackwardChars bit (T.length nname + 2) >>
                      textBufferSelectRange sb bit eit >>
                      textBufferDeleteSelection sb True True >>
                      textBufferInsertAtCursor sb (T.concat [sy,str])) >>
                      updateGState (gFunCompletion .~ Nothing)

        putCompl :: Text -> Completion -> GuiMonad ()
        putCompl str cpl = updateGState (gFunCompletion .~
                                        (Just $ addCharToCompletion str cpl))

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
                return . takeFilePath $ st ^. gFunEditBook 
    where
        takeFilePath :: FunEditBook -> Maybe TextFilePath
        takeFilePath feb = (feb ^. tabFileList) !! i

-- | Dado un EditBook, obtiene el campo de texto que esta seleccionado en
-- ese momento y su nombre.
getTextEditFromFunEditBook :: FunEditBook -> 
                              GuiMonad (Maybe (Maybe TextFilePath, String,TextView))
getTextEditFromFunEditBook feditBook = do
            let notebook = feditBook ^. book
            cPageNum       <- io $ notebookGetCurrentPage notebook
            if (cPageNum < 0) then return Nothing
            else do
              mfp            <- getTextViewFilePath cPageNum
              Just cpSW      <- io $ notebookGetNthPage notebook cPageNum
              Just textViewN <- io $ notebookGetTabLabelText notebook cpSW
              [tv]           <- io $ containerGetChildren (castToContainer cpSW)
              return (Just (mfp,textViewN,castToTextView tv))

withTextEditFromFunEditBook :: FunEditBook -> 
                              ((Maybe TextFilePath, String,TextView) -> GuiMonad ()) ->
                              GuiMonad ()
withTextEditFromFunEditBook feditBook action = getTextEditFromFunEditBook feditBook >>=
                                               maybe (return ()) action
            
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
            
            _<- io $ notebookAppendPage newnt texte name
            
            content <- ask 
            
            let editorPaned = content ^. (gFunEditorPaned . epaned)
            
            io $ panedAdd1 editorPaned newnt
            io $ widgetShowAll editorPaned
            
            return newnt
