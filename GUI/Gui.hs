-- | Interfaz gráfica de Fun.
module Main where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

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

main :: IO ()
main = do 
    initGUI
    
    xml <- fromMaybe (error msgErrGladeNotFound) <$> xmlNew "GUI/fun.glade"
    
    (gReader,gState) <- makeGState xml

    runRWST (do configWindow
                configMenuBarButtons  xml
                configInsertMenuItems xml 
                configToolBarButtons  xml
                configCommandConsole
                configSymbolList
                configAxiomList
            ) gReader gState

    mainGUI
    
-- | Configura los botones de insert de declaraciones del menu.
configInsertMenuItems :: GladeXML -> GuiMonad ()
configInsertMenuItems xml = ask >>= \content -> get >>= \st -> 
            io $ do
            specItem <- xmlGetWidget xml castToImageMenuItem "insertSpecItem"
            funItem  <- xmlGetWidget xml castToImageMenuItem "insertFunItem"
            valItem  <- xmlGetWidget xml castToImageMenuItem "insertValItem"
            -- falta implementar thmItem
            thmItem  <- xmlGetWidget xml castToImageMenuItem "insertThmItem"
            
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
makeGState :: GladeXML -> IO (GReader,GStateRef) 
makeGState xml = do
        
        symFrameB <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        
        mainPaned <- xmlGetWidget xml castToHPaned "mainPaned"
        
        declFrame  <- xmlGetWidget xml castToFrame "declFrame"
        loadedMod  <- xmlGetWidget xml castToLabel "labelLoadedModule"
        
        symFrame   <- xmlGetWidget xml castToFrame "symFrame"
        goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
        scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
        symIV      <- xmlGetWidget xml castToIconView "symbolList"
        goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
        
        axFrame  <- xmlGetWidget xml castToFrame "axiomFrame"
        axTV     <- xmlGetWidget xml castToTreeView "axiomList"
        axRel    <- xmlGetWidget xml castToComboBox "comboAxioms"
        axFrameB <- xmlGetWidget xml castToToggleToolButton "AxiomFrameButton"
        
        window <- xmlGetWidget xml castToWindow "mainWindow"
        
        edPaned <- xmlGetWidget xml castToVPaned "editorPaned"
        commTV <- xmlGetWidget xml castToTextView "commandTView"
        commEntry <- xmlGetWidget xml castToEntry "commandEntry"
        funStatusbar <- xmlGetWidget xml castToStatusbar "statusBar"
        infoTV <- xmlGetWidget xml castToTextView "infoConsoleTView"
        
        panedSetPosition edPaned 400
        
        commTBuf <- textViewGetBuffer commTV
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configInfoConsoleTV infoTV infoTBuf

        commIChan <- atomically newEmptyTMVar
        commOChan <- atomically newEmptyTMVar
        
        let funToolbarST    = FunToolbar symFrameB axFrameB
        let funMainPanedST  = FunMainPaned mainPaned
        let funInfoPanedST  = FunInfoPaned declFrame loadedMod
        let funSymListST    = FunSymList symFrame goLeftBox scrollW symIV goRightBox
        let funAxListST     = FunAxList axFrame axTV axRel
        let funCommConsole  = FunCommConsole commEntry commTBuf commTV commIChan commOChan
        let funInfoConsole  = FunInfoConsole infoTBuf infoTV
        let funEditorPaned  = FunEditorPaned edPaned
        
        
        gState <- newRef $ GState [] Nothing
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
configMenuBarButtons :: GladeXML -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st ->
        io $ do
        
        newFButton    <- xmlGetWidget xml castToToolButton "newFileButton"
        openFButton   <- xmlGetWidget xml castToToolButton "openFileButton"
        saveFButton   <- xmlGetWidget xml castToToolButton "saveFileButton"
        saveAtFButton <- xmlGetWidget xml castToToolButton "saveFileAtButton"
        closeFButton  <- xmlGetWidget xml castToToolButton "closeFileButton"
        checkMButton  <- xmlGetWidget xml castToToolButton "checkModuleButton"
        symFButton    <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        axFButton     <- xmlGetWidget xml castToToggleToolButton "AxiomFrameButton"
        
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
configToolBarButtons :: GladeXML -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
            io $ do
            let window = content ^. gFunWindow
            newB  <- xmlGetWidget xml castToMenuItem "newButton"
            openB <- xmlGetWidget xml castToMenuItem "openButton"
            saveB  <- xmlGetWidget xml castToMenuItem "saveButton"
            saveAsB <- xmlGetWidget xml castToMenuItem "saveAsButton"
            closeB  <- xmlGetWidget xml castToMenuItem "closeButton"
            quitB  <- xmlGetWidget xml castToMenuItem "quitButton"
            
            checkB <- xmlGetWidget xml castToMenuItem "checkButton"
            
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
