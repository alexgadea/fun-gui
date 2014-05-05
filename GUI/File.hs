{-# Language DoAndIfThenElse #-}
module GUI.File where

import Graphics.UI.Gtk hiding (get)

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS

import qualified Data.Foldable as F
import System.FilePath.Posix
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text hiding (take,init,drop,null,head)

import GUI.GState
import GUI.DeclList
import GUI.EditBook
import GUI.Utils
import GUI.InfoConsole
import GUI.EvalConsole(resetEnv)

import Fun.Environment
import Fun.Parser
import Fun.Module(allDeclsValid,modName,ModName)

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
            updateGState ((.~) gFunEditBook (Just $ FunEditBook editBook [mfp]))
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
            (updateGState ((.~) gFunEditBook 
                                (Just $ FunEditBook ebook (fileList ++ [mfp]))))

-- | Crea un nuevo archivo en blanco.
createNewFile :: GuiMonad ()
createNewFile = createNewFileFromLoad Nothing Nothing Nothing

-- | Cierra el archivo presente.
closeCurrentFile :: GuiMonad ()
closeCurrentFile = getGState >>= \st ->
        when (isJust (st ^. gFunEditBook)) $ do
           let (Just editBook) = st ^. gFunEditBook
           let ebook    = editBook ^. book
           let fileList = editBook ^. tabFileList
           let env = st ^. gFunEnv 
           (_,myModName,_) <- getTextEditFromFunEditBook editBook
           cPageNum  <- io $ notebookGetCurrentPage ebook
           io $ notebookRemovePage ebook cPageNum
           quantPages <- io $ notebookGetNPages ebook
           -- Si el archivo está cargado, eliminamos todas las declaraciones
           -- de la barra de la izquierda.
           unless (null env || pack myModName /= head env ^. modName) (updEnv [] Nothing)
           -- Si no quedan módulos abiertos, cerramos el EditBook.
           if (quantPages == 0) 
           then updateGState (gFunEditBook .~ Nothing)
           else do let updateFileList = upList cPageNum fileList
                   updateGState ((.~) gFunEditBook (Just $ FunEditBook ebook updateFileList))
                   
    where upList :: Int -> [a] -> [a]
          upList n ls = take n ls ++ drop (n+1) ls

-- | Chequea un archivo cargado, esto implica parsearlo, typechequearlo y
-- validarlo.
checkSelectFile :: GuiMonad ()
checkSelectFile = 
    getGState >>= \st ->
    when (isJust $ st ^. gFunEditBook) $ do 
        let (Just editBook) = st ^. gFunEditBook
        (mfp,_,_) <- getTextEditFromFunEditBook editBook
        if (not $ isJust mfp)
        then saveAtFile
        else (io . loadMainModuleFromFile) (fromJust mfp) >>=
                either notLoadModule (loadModule editBook)

  where loadModule eb (env, name) = do updEnv env (Just name)
                                       let (Just mainm) = getModule env name
                                       printInfoMsg (msgModule mainm name)
                                       resetEnv
                                       updateModulesFunEditBook eb name
        notLoadModule err = updEnv [] Nothing >> printErrorMsg (show err)
        msgModule m name | allDeclsValid m = "Módulo "++ unpack name ++ " cargado."
                         | otherwise = "Módulo "++ unpack name ++ " cargado con errores."

updEnv :: Environment -> Maybe ModName -> GuiMonad ()
updEnv env mname = updateGState (gFunEnv .~ env) >>
                   updateInfoPaned env mname >>
                   updateEvalEnv

-- | Función para cargar un archivo en base a un filepath.
openFileFromPath :: TextFilePath -> GuiMonad ()
openFileFromPath fp = ask >>= \ct -> get >>= \st -> io $
        readFile (unpack fp) >>= \code -> 
        takeFilepathName (unpack fp) >>= \fileName ->
        createFromFile ct st (Just fp) (Just fileName) (Just code) >>
        return ()

-- | Función para cargar un archivo.
openFile :: GuiMonad ()
openFile = ask >>= \ct -> get >>= \st ->
    io $ dialogLoad "Cargar programa" funFileFilter (createFromFile ct st) >>
    return ()

-- | Función mas general para crear una pestaña con codigo en base a un
-- filepath, nombre de pestaña o codigo en un string.
createFromFile :: GReader -> GStateRef -> Maybe TextFilePath ->
                  Maybe String -> Maybe String -> IO ()
createFromFile content st mfp mname mcode = 
    evalRWST (createNewFileFromLoad mfp mname mcode) content st >> return ()

-- | Dialogo general para la carga de archivos.
dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
              (Maybe TextFilePath -> Maybe String -> Maybe String -> IO ()) -> 
              IO Bool
dialogLoad dlabel fileFilter action = do
    dialog <- fileChooserDialogNew (Just dlabel) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    dResponse <- dialogRun dialog
    
    case dResponse of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            F.mapM_ (\filepath -> 
                    readFile filepath >>= \code ->
                    takeFilepathName filepath >>= \fileName ->
                    action (Just $ pack filepath) (Just fileName) (Just code) >>
                    widgetDestroy dialog) selected 
            return True
        _ -> widgetDestroy dialog >> return False

takeFilepathName :: FilePath -> IO String
takeFilepathName = return . takeBaseName

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
                updateGState ((.~) gFunEditBook (Just $ FunEditBook ebook updateFileList))
        upList :: FilePath -> Int -> [Maybe TextFilePath] -> [Maybe TextFilePath]
        upList fp n ls = (init $ take (n+1) ls) ++ [Just $ pack fp] ++ (drop (n+1) ls)

-- | Dialogo general para guardar un archivo.
saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
              String -> GuiMonad (Maybe FilePath)
saveDialog dlabel filename fileFilter serialItem = do
        dialog <- io $ fileChooserDialogNew (Just dlabel) 
                                            Nothing 
                                            FileChooserActionSave 
                                            [ ("Guardar",ResponseAccept)
                                            , ("Cancelar",ResponseCancel)
                                            ]
        
        io $ fileChooserSetCurrentName dialog filename
        io $ fileFilter dialog
        dResponse <- io $ dialogRun dialog

        case dResponse of
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
