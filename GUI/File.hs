module GUI.File where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Foldable as F
import System.FilePath.Posix
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text hiding (take,init,drop)

import GUI.GState
import GUI.DeclList
import GUI.EditBook
import GUI.Utils
import GUI.InfoConsole
import GUI.EvalConsole(resetEnv)

import Fun.Environment
import Fun.Parser
import Fun.Module.Error
import Fun.Module (ModName)

import Lens.Family

-- | En general, salvo aclaración, un archivo en este contexto es directamente
-- un campo de texto con su respectivo nombre en la interfaz.

-- | Crea un campo de texto al realizar una carga por archivo.
createNewFileFromLoad :: Maybe TextFilePath -> Maybe String -> Maybe String -> 
                         GuiMonad ()
createNewFileFromLoad mfp mname mcode = getGState >>= \st -> ask >>= \content ->
    case st ^. gFunEditBook of
        Nothing -> 
            let editorPaned = content ^. (gFunEditorPaned . epaned) in
            io (panedGetChild1 editorPaned) >>= \(Just drawArea) ->
            io (containerRemove (castToContainer editorPaned) drawArea) >>
            createEditBook mname mcode >>= \editBook -> 
            updateGState ((<~) gFunEditBook (Just $ FunEditBook editBook [mfp]))
        Just editBook -> 
            let ebook = editBook ^. book
                fileList = editBook ^. tabFileList
            in
            createTextEdit mcode >>= \textEdit ->
            (\name -> 
            io (notebookAppendPage ebook textEdit name) >>
            io (notebookGetNPages ebook) >>= \nPages ->
            io (notebookSetCurrentPage ebook (nPages-1)) >>
            return ()) (fromMaybe "blank" mname) >>
            (updateGState ((<~) gFunEditBook 
                                (Just $ FunEditBook ebook (fileList ++ [mfp]))))

-- | Crea un nuevo archivo en blanco.
createNewFile :: GuiMonad ()
createNewFile = createNewFileFromLoad Nothing Nothing Nothing

-- | Cierra el archivo presente.
closeCurrentFile :: GuiMonad ()
closeCurrentFile = getGState >>= \st ->
        case st ^. gFunEditBook of
            Nothing -> return ()
            Just editBook -> 
                do
                let ebook    = editBook ^. book
                let fileList = editBook ^. tabFileList
                cPageNum  <- io $ notebookGetCurrentPage ebook
                io $ notebookRemovePage ebook cPageNum
                quantPages <- io $ notebookGetNPages ebook
                if (quantPages == 0) 
                    then updateGState (gFunEditBook <~ Nothing)
                    else do
                        let updateFileList = upList cPageNum fileList
                        updateGState ((<~) gFunEditBook (Just $ FunEditBook ebook updateFileList))
    where
        upList :: Int -> [a] -> [a]
        upList n ls = (init $ take (n+1) ls) ++ (drop (n+1) ls)

-- | Chequea un archivo cargando, esto implica parsearlo, typechequearlo y
-- validarlo.
checkSelectFile :: GuiMonad ()
checkSelectFile = 
    getGState >>= \st ->
    ask >>= \content ->
    let chan = content ^. (gFunCommConsole . commChan) in
    let repChan = content ^. (gFunCommConsole . commRepChan) in
    case st ^. gFunEditBook of
    Nothing -> return ()
    Just editBook -> do
        (mfp,_,_) <- getTextEditFromFunEditBook editBook
        if (not $ isJust mfp) 
            then saveAtFile
            else do
                eRes <- io $ loadMainModuleFromFile $ fromJust mfp
                either (\err -> updEnv [] Nothing >> printErrorMsg (show err)) 
                        (\(env,mName) -> updEnv env (Just mName) >> 
                                printInfoMsg "Módulo Cargado." >>
                                resetEnv >>
                                updateModulesFunEditBook editBook mName) eRes
    where
        updEnv env mname = updateGState (gFunEnv <~ env) >> updateInfoPaned env mname

-- | Función para cargar un archivo.
openFile :: GuiMonad ()
openFile = ask >>= \ct -> get >>= \st ->
           io $ dialogLoad "Cargar programa" funFileFilter (openFile' ct st) >>
            return ()
    where
        openFile' :: GReader -> GStateRef -> Maybe TextFilePath ->
                     Maybe String -> Maybe String -> IO ()
        openFile' content st mfp mname mcode = 
            evalRWST (createNewFileFromLoad mfp mname mcode) content st >> 
            return ()

-- | Dialogo general para la carga de archivos.
dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
              (Maybe TextFilePath -> Maybe String -> Maybe String -> IO ()) -> 
              IO Bool
dialogLoad label fileFilter action = do
    dialog <- fileChooserDialogNew (Just label) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    response <- dialogRun dialog
    
    case response of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            F.mapM_ (\filepath -> 
                    readFile filepath >>= \code ->
                    takeFileName filepath >>= \fileName ->
                    action (Just $ pack filepath) (Just fileName) (Just code) >>
                    widgetDestroy dialog) selected 
            return True
        _ -> widgetDestroy dialog >> return False
    where
        takeFileName :: FilePath -> IO String
        takeFileName = return . takeBaseName

-- | Generador de filtros para la carga y guardado de archivos.
setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
                                hsfilt <- fileFilterNew
                                mapM_ (fileFilterAddPattern hsfilt) patterns
                                fileFilterSetName hsfilt title
                                fileChooserAddFilter fChooser hsfilt    

-- | Guardado directo de un archivo.
saveFile :: GuiMonad ()
saveFile = getGState >>= \st ->
        case st ^. gFunEditBook of
            Nothing -> return ()
            Just editBook -> do
                (mfp,_,textV) <- getTextEditFromFunEditBook editBook
                case mfp of
                    Nothing -> saveAtFile
                    Just fp -> getCode textV >>= save (unpack fp)
    where
        save:: FilePath -> String -> GuiMonad ()
        save filepath code = io $ writeFile filepath code
        getCode :: TextView -> GuiMonad String
        getCode textV = io $ do
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            textBufferGetText buf start end False

-- | Guardado en, de un archivo.
saveAtFile :: GuiMonad ()
saveAtFile = getGState >>= \st ->
        case st ^. gFunEditBook of
            Nothing -> return ()
            Just editBook -> do
                (_,name,textV) <- getTextEditFromFunEditBook editBook
                code <- getCode textV
                let nFile = if name == "blank" then "" else name
                mfp <- saveDialog "Guardar programa" (nFile++".fun") funFileFilter code
                when (isJust mfp) (updateFL editBook $ fromJust mfp)
                return ()
    where
        getCode :: TextView -> GuiMonad String
        getCode textV = io $ do
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            textBufferGetText buf start end False
        updateFL :: FunEditBook -> FilePath -> GuiMonad ()
        updateFL editBook fp = do
                let ebook    = editBook ^. book
                let fileList = editBook ^. tabFileList
                cPageNum  <- io $ notebookGetCurrentPage ebook
                let updateFileList = upList fp cPageNum fileList
                updateGState ((<~) gFunEditBook (Just $ FunEditBook ebook updateFileList))
        upList :: FilePath -> Int -> [Maybe TextFilePath] -> [Maybe TextFilePath]
        upList fp n ls = (init $ take (n+1) ls) ++ [Just $ pack fp] ++ (drop (n+1) ls)

-- | Dialogo general para guardar un archivo.
saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
              String -> GuiMonad (Maybe FilePath)
saveDialog label filename fileFilter serialItem = do
        dialog <- io $ fileChooserDialogNew (Just label) 
                                            Nothing 
                                            FileChooserActionSave 
                                            [ ("Guardar",ResponseAccept)
                                            , ("Cancelar",ResponseCancel)
                                            ]
        
        io $ fileChooserSetCurrentName dialog filename
        io $ fileFilter dialog
        response <- io $ dialogRun dialog

        case response of
            ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
                              \fp -> F.mapM_ save fp >> 
                              io (widgetDestroy dialog) >> 
                              return fp
            _ -> io (widgetDestroy dialog) >> return Nothing
    where
        save:: FilePath -> GuiMonad ()
        save filepath = io $ writeFile filepath serialItem

-- | Filtro de programas de fun.
funFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
funFileFilter dialog = io $ setFileFilter dialog ["*.fun"] "Programa de fun"
