module GUI.File where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.RWS
import qualified Data.Foldable as F
import Data.String.Utils

import GUI.GState
import GUI.EditBook

import Fun.Environment
import Fun.Module.Error

import Lens.Family

createNewFileFromLoad :: Maybe String -> Maybe String -> GuiMonad ()
createNewFileFromLoad mname mcode = getGState >>= \st -> ask >>= \content ->
        do
        case st ^. gFunEditBook of
            Nothing -> 
                let mainPaned = content ^. (gFunMainPaned . mpaned) in
                io (panedGetChild2 mainPaned) >>= \(Just drawArea) ->
                io (containerRemove (castToContainer mainPaned) drawArea) >>
                createEditBook mname mcode >>= \editBook -> 
                updateGState ((^=) gFunEditBook (Just $ FunEditBook editBook))
            Just editBook -> let ebook = editBook ^. book in
                createTextEdit mcode >>= \textEdit ->
                return (maybe "blank" id mname) >>= \name -> 
                io (notebookAppendPage ebook textEdit name) >>
                io (notebookGetNPages ebook) >>= \nPages ->
                io (notebookSetCurrentPage ebook (nPages-1)) >>
                return ()

createNewFile :: GuiMonad ()
createNewFile = createNewFileFromLoad Nothing Nothing
    
closeCurrentFile :: GuiMonad ()
closeCurrentFile = getGState >>= \st -> do
        case st ^. gFunEditBook of
            Nothing -> return ()
            Just editBook -> do
                             let ebook = editBook ^. book
                             cPageNum  <- io $ notebookGetCurrentPage ebook
                             io $ notebookRemovePage ebook cPageNum
                             quantPages <- io $ notebookGetNPages ebook
                             when (quantPages == 0) 
                                  (updateGState ((^=) gFunEditBook Nothing))

checkSelectFile :: GuiMonad ()
checkSelectFile = getGState >>= \st ->
                case st ^. gFunEditBook of
                    Nothing -> return ()
                    Just editBook -> do
                        (_,textV) <- getTextEditFromFunEditBook editBook
                        eRes <- check textV
                        either (io . putStrLn . show) 
                               (\env -> updateGState ((^=) gFunEnv env)) eRes
    where
        check :: TextView -> GuiMonad (Either ModuleError Environment)
        check textV = io $ do
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            code  <- textBufferGetText buf start end False
            eRes  <- loadMainModuleFromString code
            return eRes


openFile :: GuiMonad ()
openFile = ask >>= \ct -> get >>= \st ->
           io $ dialogLoad "Cargar programa" funFileFilter (openFile' ct st) >>
            return ()
    where
        openFile' :: GReader -> GStateRef -> 
                     Maybe String -> Maybe String -> IO ()
        openFile' content st mname mcode = 
                    evalRWST (createNewFileFromLoad mname mcode) content st >> 
                    return ()

dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
             (Maybe String -> Maybe String -> IO ()) -> IO Bool
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
            flip F.mapM_ selected (\filepath -> 
                                    readFile filepath >>= \code ->
                                    takeFileName filepath >>= \fileName ->
                                    action (Just fileName) (Just code) >>
                                    widgetDestroy dialog)
            return True
        _ -> widgetDestroy dialog >> return False
    where
        takeFileName :: FilePath -> IO String
        takeFileName = return . head . split "." . last . split "/"
    
setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
                                hsfilt <- fileFilterNew
                                mapM_ (fileFilterAddPattern hsfilt) patterns
                                fileFilterSetName hsfilt title
                                fileChooserAddFilter fChooser hsfilt    
    
saveFile :: GuiMonad ()
saveFile = return ()
            
saveAtFile :: GuiMonad ()
saveAtFile = getGState >>= \st ->
        case st ^. gFunEditBook of
            Nothing -> return ()
            Just editBook -> do
                (name,textV) <- getTextEditFromFunEditBook editBook
                code <- getCode textV
                let nFile = if name == "blank" then "" else name
                saveDialog "Guardar programa" (nFile++".fun") funFileFilter code
                return ()
    where
        getCode :: TextView -> GuiMonad String
        getCode textV = io $ do
            buf   <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end   <- textBufferGetEndIter buf
            textBufferGetText buf start end False

saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
              String -> GuiMonad Bool
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
                            io (widgetDestroy dialog) >> return True
            _ -> io (widgetDestroy dialog) >> return False
    where
        save:: FilePath -> GuiMonad ()
        save filepath = io $ writeFile filepath serialItem

funFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
funFileFilter dialog = io $ setFileFilter dialog ["*.fun"] "Programa de fun"
