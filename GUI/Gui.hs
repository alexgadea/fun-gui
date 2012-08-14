-- | Interfaz gráfica de Equ.
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
import Data.Maybe (fromJust,isJust)
import Data.Reference

import qualified Equ.Parser as EquParser

import GUI.File
import GUI.GState
import GUI.EditBook
import GUI.File
import GUI.Config
import GUI.SymbolList
import GUI.EvalConsole
import GUI.InfoConsole
import GUI.Utils
import qualified GUI.InsertDialogs as IDialogs

main :: IO ()
main = do 
    initGUI
    
--     (greader,gstate) <- makeGState "GUI/fun.glade"
    
    Just xml <- xmlNew "GUI/fun.glade"

    newFB <- xmlGetWidget xml castToToolButton "newFileButton"
    openFB <- xmlGetWidget xml castToToolButton "openFileButton"
    saveFB <- xmlGetWidget xml castToToolButton "saveFileButton"
    saveAtFB <- xmlGetWidget xml castToToolButton "saveFileAtButton"
    closeFB <- xmlGetWidget xml castToToolButton "closeFileButton"
    checkMB <- xmlGetWidget xml castToToolButton "checkModuleButton"
    symFrameB <- xmlGetWidget xml castToToggleToolButton "symFrameButton"

    -- items
    insertSpecItem <- xmlGetWidget xml castToImageMenuItem "insertSpecItem"
    insertFunItem <- xmlGetWidget xml castToImageMenuItem "insertFunItem"
    insertValItem <- xmlGetWidget xml castToImageMenuItem "insertValItem"
    insertThmItem <- xmlGetWidget xml castToImageMenuItem "insertThmItem"

    mainPaned <- xmlGetWidget xml castToHPaned "mainPaned"

    iSpecs <- xmlGetWidget xml castToExpander "expSpecs"
    iFuncs <- xmlGetWidget xml castToExpander "expFuncs"
    iThms  <- xmlGetWidget xml castToExpander "expThms"
    iVals  <- xmlGetWidget xml castToExpander "expVals"
    iProps <- xmlGetWidget xml castToExpander "expProps"
    loadedMod <- xmlGetWidget xml castToLabel "labelLoadedModule"

    symFrame   <- xmlGetWidget xml castToFrame "symFrame"
    goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
    scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
    symIV      <- xmlGetWidget xml castToIconView "symbolList"
    goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"

    window <- xmlGetWidget xml castToWindow "mainWindow"
    quitButton <- xmlGetWidget xml castToMenuItem "quitButton"

    edPaned <- xmlGetWidget xml castToVPaned "editorPaned"
    commTV <- xmlGetWidget xml castToTextView "commandTView"
    commEntry <- xmlGetWidget xml castToEntry "commandEntry"

    infoTV <- xmlGetWidget xml castToTextView "infoConsoleTView"

    panedSetPosition edPaned 400

    commTBuf <- textViewGetBuffer commTV

    infoTBuf <- textViewGetBuffer infoTV

    configInfoConsoleTV infoTV infoTBuf
    
    commIChan <- atomically newEmptyTMVar
    commOChan <- atomically newEmptyTMVar

    let funFunMenuBarST = FunMenuBar quitButton
    let funToolbarST    = FunToolbar newFB openFB saveFB saveAtFB closeFB checkMB symFrameB
    let funMainPanedST  = FunMainPaned mainPaned
    let funInfoPanedST  = FunInfoPaned iSpecs iFuncs iThms iVals iProps loadedMod
    let funSymListST    = FunSymList symFrame goLeftBox scrollW symIV goRightBox
    let funCommConsole  = FunCommConsole commEntry commTBuf commTV commIChan commOChan
    let funInfoConsole  = FunInfoConsole infoTBuf infoTV
    let funEditorPaned  = FunEditorPaned edPaned

    gState <- newRef $ GState [] Nothing
    
    let gReader = GReader window 
                          funFunMenuBarST 
                          funToolbarST 
                          funMainPanedST
                          funInfoPanedST
                          funSymListST
                          funEditorPaned
                          funCommConsole
                          funInfoConsole

    runRWST (do configWindow
                configInsertMenuItems insertSpecItem insertFunItem insertValItem insertThmItem
                configToolBarButtons
                configMenuBarButtons
                configCommandConsole
                configSymbolList
            ) gReader gState

    mainGUI


configInsertMenuItems :: ImageMenuItem -> ImageMenuItem -> 
                         ImageMenuItem -> ImageMenuItem -> GuiMonad ()
configInsertMenuItems specItem funItem valItem thmItem =
    ask >>= \content -> get >>= \st -> io $ 
    onActivateLeaf specItem (evalRWST (IDialogs.createDeclDialog IDialogs.Spec >>=
                                       IDialogs.runDialog) content st >> return ()) >>
    onActivateLeaf funItem (evalRWST (IDialogs.createDeclDialog IDialogs.Fun >>=
                                       IDialogs.runDialog) content st >> return ()) >>
    onActivateLeaf valItem (evalRWST (IDialogs.createDeclDialog IDialogs.Val >>=
                                       IDialogs.runDialog) content st >> return ()) >>
    return ()
    -- falta implementar thmItem

makeGState :: String -> IO (GReader,GStateRef) 
makeGState sXml = do
        Just xml <- xmlNew sXml
        
        newFB <- xmlGetWidget xml castToToolButton "newFileButton"
        openFB <- xmlGetWidget xml castToToolButton "openFileButton"
        saveFB <- xmlGetWidget xml castToToolButton "saveFileButton"
        saveAtFB <- xmlGetWidget xml castToToolButton "saveFileAtButton"
        closeFB <- xmlGetWidget xml castToToolButton "closeFileButton"
        checkMB <- xmlGetWidget xml castToToolButton "checkModuleButton"
        symFrameB <- xmlGetWidget xml castToToggleToolButton "symFrameButton"
        
        mainPaned <- xmlGetWidget xml castToHPaned "mainPaned"
        
        iSpecs <- xmlGetWidget xml castToExpander "expSpecs"
        iFuncs <- xmlGetWidget xml castToExpander "expFuncs"
        iThms  <- xmlGetWidget xml castToExpander "expThms"
        iVals  <- xmlGetWidget xml castToExpander "expVals"
        iProps <- xmlGetWidget xml castToExpander "expProps"
        loadedMod <- xmlGetWidget xml castToLabel "labelLoadedModule"
        
        symFrame   <- xmlGetWidget xml castToFrame "symFrame"
        goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
        scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
        symIV      <- xmlGetWidget xml castToIconView "symbolList"
        goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
        
        window <- xmlGetWidget xml castToWindow "mainWindow"
        quitButton <- xmlGetWidget xml castToMenuItem "quitButton"
        
        edPaned <- xmlGetWidget xml castToVPaned "editorPaned"
        commTV <- xmlGetWidget xml castToTextView "commandTView"
        commEntry <- xmlGetWidget xml castToEntry "commandEntry"
        
        infoTV <- xmlGetWidget xml castToTextView "infoConsoleTView"
        
        panedSetPosition edPaned 400
        
        commTBuf <- textViewGetBuffer commTV
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configInfoConsoleTV infoTV infoTBuf

        commIChan <- atomically newEmptyTMVar
        commOChan <- atomically newEmptyTMVar
        
        let funFunMenuBarST = FunMenuBar quitButton
        let funToolbarST    = FunToolbar newFB openFB saveFB saveAtFB closeFB checkMB symFrameB
        let funMainPanedST  = FunMainPaned mainPaned
        let funInfoPanedST  = FunInfoPaned iSpecs iFuncs iThms iVals iProps loadedMod
        let funSymListST    = FunSymList symFrame goLeftBox scrollW symIV goRightBox
        let funCommConsole  = FunCommConsole commEntry commTBuf commTV commIChan commOChan
        let funInfoConsole  = FunInfoConsole infoTBuf infoTV
        let funEditorPaned  = FunEditorPaned edPaned
        
        
        gState <- newRef $ GState [] Nothing
        let gReader = GReader window 
                              funFunMenuBarST 
                              funToolbarST 
                              funMainPanedST
                              funInfoPanedST
                              funSymListST
                              funEditorPaned
                              funCommConsole
                              funInfoConsole
        return (gReader,gState)

configMenuBarButtons :: GuiMonad ()
configMenuBarButtons = ask >>= \content -> get >>= \st ->
        liftIO $ do
        let newFButton    = content ^. (gFunToolbar . newFB)
        let openFButton   = content ^. (gFunToolbar . openFB)
        let saveFButton   = content ^. (gFunToolbar . saveFB)
        let saveAtFButton = content ^. (gFunToolbar . saveAtFB)
        let closeFButton  = content ^. (gFunToolbar . closeFB)
        let checkMButton  = content ^. (gFunToolbar . checkMB)
        let symFButton    = content ^. (gFunToolbar . symFrameB)
        
        onToolButtonClicked newFButton    (eval createNewFile content st)
        onToolButtonClicked openFButton   (eval openFile content st)
        onToolButtonClicked saveFButton   (eval saveFile content st)
        onToolButtonClicked saveAtFButton (eval saveAtFile content st)
        onToolButtonClicked closeFButton  (eval closeCurrentFile content st)
        onToolButtonClicked checkMButton  (eval checkSelectFile content st)
        onToolButtonClicked symFButton    (eval configSymFrameButton content st)
            
        return ()
    where
        eval :: GuiMonad () -> GReader -> GStateRef -> IO ()
        eval action content str = evalRWST action content str >> return ()

configToolBarButtons :: GuiMonad ()
configToolBarButtons = ask >>= \content ->
        liftIO $ do
        let (quitB,window) = flip (^.) (gFunMenuBar . quitButton) 
                             &&& 
                             flip (^.) gFunWindow $ content
        onActivateLeaf quitB $ widgetDestroy window
        return ()

configWindow :: GuiMonad ()
configWindow = ask >>= \content -> 
        liftIO $ do
        let window = content ^. gFunWindow
        windowMaximize window
        widgetShowAll window
        onDestroy window mainQuit
        return ()
