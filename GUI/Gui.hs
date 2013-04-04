-- | Interfaz gráfica de Fun.
module GUI.Gui where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.IO.Class
import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,isJust,fromMaybe)
import Data.Reference

import qualified Equ.Parser as EquParser

import GUI.File
import GUI.GState
import GUI.EditBook
import GUI.Config
import GUI.SymbolList
import GUI.AxiomList
import GUI.EvalConsole
import GUI.InfoConsole
import GUI.Utils
import qualified GUI.InsertDialogs as IDialogs

mainFunGui :: Builder -> IO (GReader, GStateRef)
mainFunGui xml = do 
                (gReader,gState) <- makeGState xml

                (_,stRef,_) <- runRWST (do  configWindow
                                            configMenuBarButtons  xml
                                            configInsertMenuItems xml 
                                            configToolBarButtons  xml
                                            configCommandConsole
                                            configSymbolList
                                            configAxiomList
                                        ) gReader gState
                return (gReader,stRef)

-- | Configura los botones de insert de declaraciones del menu.
configInsertMenuItems :: Builder -> GuiMonad ()
configInsertMenuItems xml = ask >>= \content -> get >>= \st -> 
            io $ do
            specItem <- builderGetObject xml castToImageMenuItem "insertSpecItem"
            funItem  <- builderGetObject xml castToImageMenuItem "insertFunItem"
            valItem  <- builderGetObject xml castToImageMenuItem "insertValItem"
            -- falta implementar thmItem
            thmItem  <- builderGetObject xml castToImageMenuItem "insertThmItem"
            
            onActivateLeaf specItem (createRun IDialogs.Spec content st)
            onActivateLeaf funItem  (createRun IDialogs.Fun  content st)
            onActivateLeaf valItem  (createRun IDialogs.Val  content st)
            
            return ()
    where
        createRunDialog :: IDialogs.DeclType -> GuiMonad ()
        createRunDialog dtype = IDialogs.createDeclDialog dtype >>= IDialogs.runDialog
        createRun :: IDialogs.DeclType -> GReader -> GStateRef -> IO ()
        createRun dtype content str = do
                                evalRWST (createRunDialog dtype) content str
                                return ()

-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (GReader,GStateRef) 
makeGState xml = do
        
        symFrameB <- builderGetObject xml castToToggleToolButton "symFrameButton"
        
        mainPaned <- builderGetObject xml castToHPaned "mainPaned"
        
        declFrame  <- builderGetObject xml castToFrame "declFrame"
        loadedMod  <- builderGetObject xml castToLabel "labelLoadedModule"
        
        symFrame   <- builderGetObject xml castToFrame "symFrame"
        goLeftBox  <- builderGetObject xml castToHBox "symGoLeftBox"
        scrollW    <- builderGetObject xml castToScrolledWindow "swSymbolList"
        symIV      <- builderGetObject xml castToIconView "symbolList"
        goRightBox <- builderGetObject xml castToHBox "symGoRightBox"
        
        axFrame  <- builderGetObject xml castToFrame "axiomFrame"
        axTV     <- builderGetObject xml castToTreeView "axiomList"
        axRel    <- builderGetObject xml castToComboBox "comboAxioms"
        axFrameB <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        axLabExpr <- builderGetObject xml castToLabel "axiomExpr"
        
        window <- builderGetObject xml castToWindow "mainWindow"
        
        edPaned <- builderGetObject xml castToVPaned "editorPaned"
        commTV <- builderGetObject xml castToTextView "commandTView"
        commEntry <- builderGetObject xml castToEntry "commandEntry"
        funStatusbar <- builderGetObject xml castToStatusbar "statusBar"
        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        
        panedSetPosition edPaned 400
        
        commTBuf <- textViewGetBuffer commTV
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configInfoConsoleTV infoTV infoTBuf
{-
        commIChan <- atomically newEmptyTMVar
        commOChan <- atomically newEmptyTMVar
        -}
        
        let funToolbarST    = FunToolbar symFrameB axFrameB
        let funMainPanedST  = FunMainPaned mainPaned
        let funInfoPanedST  = FunInfoPaned declFrame loadedMod
        let funSymListST    = FunSymList symFrame goLeftBox scrollW symIV goRightBox
        let funAxListST     = FunAxList axFrame axTV axRel axLabExpr
        let funCommConsole  = FunCommConsole commEntry commTBuf commTV
        let funInfoConsole  = FunInfoConsole infoTBuf infoTV
        let funEditorPaned  = FunEditorPaned edPaned
        
        
        gState <- newRef $ GState [] Nothing (FunEvalState Nothing initEvalEnv Nothing)
        let gReader = GReader window 
                              funToolbarST
                              funMainPanedST
                              funInfoPanedST
                              funSymListST
                              funAxListST
                              funStatusbar 
                              funEditorPaned
                              funCommConsole
                              funInfoConsole
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        newFButton    <- builderGetObject xml castToToolButton "newFileButton"
        openFButton   <- builderGetObject xml castToToolButton "openFileButton"
        saveFButton   <- builderGetObject xml castToToolButton "saveFileButton"
        saveAtFButton <- builderGetObject xml castToToolButton "saveFileAtButton"
        closeFButton  <- builderGetObject xml castToToolButton "closeFileButton"
        checkMButton  <- builderGetObject xml castToToolButton "checkModuleButton"
        symFButton    <- builderGetObject xml castToToggleToolButton "symFrameButton"
        axFButton     <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        
        onToolButtonClicked newFButton    (eval createNewFile content st)
        onToolButtonClicked openFButton   (eval openFile content st)
        onToolButtonClicked saveFButton   (eval saveFile content st)
        onToolButtonClicked saveAtFButton (eval saveAtFile content st)
        onToolButtonClicked closeFButton  (eval closeCurrentFile content st)
        onToolButtonClicked checkMButton  (eval checkSelectFile content st)
        onToolButtonClicked symFButton    (eval configSymFrameButton content st)
        onToolButtonClicked axFButton     (eval configAxFrameButton content st)
        
        return ()

-- | Configura los botones del menude archivo.
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
            io $ do
            let window = content ^. gFunWindow
            newB  <- builderGetObject xml castToMenuItem "newButton"
            openB <- builderGetObject xml castToMenuItem "openButton"
            saveB  <- builderGetObject xml castToMenuItem "saveButton"
            saveAsB <- builderGetObject xml castToMenuItem "saveAsButton"
            closeB  <- builderGetObject xml castToMenuItem "closeButton"
            quitB  <- builderGetObject xml castToMenuItem "quitButton"
            
            checkB <- builderGetObject xml castToMenuItem "checkButton"
            
            onActivateLeaf newB   $ eval createNewFile    content st
            onActivateLeaf openB   $ eval openFile    content st
            onActivateLeaf saveB  $ eval saveFile         content st
            onActivateLeaf saveAsB  $ eval saveAtFile         content st
            onActivateLeaf closeB $ eval closeCurrentFile content st
            onActivateLeaf quitB  $ widgetDestroy window
            
            onActivateLeaf checkB $ eval checkSelectFile content st
            return ()

-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> 
            io $ do
            let window = content ^. gFunWindow
            windowMaximize window
            widgetShowAll window
            onDestroy window mainQuit
            return ()
            
eval :: GuiMonad () -> GReader -> GStateRef -> IO ()
eval action content str = evalRWST action content str >> return ()

-- | Mensaje de error en caso de no encontrar el archivo glade correspondiente.
msgErrGladeNotFound :: String
msgErrGladeNotFound = "Archivo fun.glade no encontrado"
