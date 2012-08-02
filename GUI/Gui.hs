-- | Interfaz gráfica de Equ.
module Main where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,isJust)
import Data.Reference

import GUI.File
import GUI.GState
import GUI.EditBook
import GUI.File
import GUI.Config
import GUI.SymbolList

main :: IO ()
main = do 
    initGUI
    
    (greader,gstate) <- makeGState "GUI/fun.glade"
    
    runRWST (do configWindow
                configToolBarButtons
                configMenuBarButtons
                configCommandConsole
                configSymbolList
            ) greader gstate

    mainGUI

makeGState :: String -> IO (GReader,GStateRef) 
makeGState sXml = do
        Just xml <- xmlNew sXml
        
        newFB <- xmlGetWidget xml castToToolButton "newFileButton"
        openFB <- xmlGetWidget xml castToToolButton "openFileButton"
        saveFB <- xmlGetWidget xml castToToolButton "saveFileButton"
        saveAtFB <- xmlGetWidget xml castToToolButton "saveFileAtButton"
        closeFB <- xmlGetWidget xml castToToolButton "closeFileButton"
        checkMB <- xmlGetWidget xml castToToolButton "checkModuleButton"
        
        mainPaned <- xmlGetWidget xml castToHPaned "mainPaned"
        
        iSpecs <- xmlGetWidget xml castToExpander "expSpecs"
        iFuncs <- xmlGetWidget xml castToExpander "expFuncs"
        iThms  <- xmlGetWidget xml castToExpander "expThms"
        iVals  <- xmlGetWidget xml castToExpander "expVals"
        iProps <- xmlGetWidget xml castToExpander "expProps"
        
        goLeftBox  <- xmlGetWidget xml castToHBox "symGoLeftBox"
        scrollW    <- xmlGetWidget xml castToScrolledWindow "swSymbolList"
        symIV      <- xmlGetWidget xml castToIconView "symbolList"
        goRightBox <- xmlGetWidget xml castToHBox "symGoRightBox"
        
        window <- xmlGetWidget xml castToWindow "mainWindow"
        quitButton <- xmlGetWidget xml castToMenuItem "quitButton"
        
        edPaned <- xmlGetWidget xml castToVPaned "editorPaned"
        commTV <- xmlGetWidget xml castToTextView "commandTView"
        commEntry <- xmlGetWidget xml castToEntry "commandEntry"
        
        panedSetPosition edPaned 400
        configCommTV commTV
        
        commTBuf <- textViewGetBuffer commTV
        
        let funFunMenuBarST = FunMenuBar quitButton
        let funToolbarST = FunToolbar newFB openFB saveFB saveAtFB closeFB checkMB
        let funMainPanedST = FunMainPaned mainPaned
        let funInfoPanedST = FunInfoPaned iSpecs iFuncs iThms iVals iProps
        let funSymListST    = FunSymList goLeftBox scrollW symIV goRightBox
        let funCommConsole = FunCommConsole commEntry commTBuf commTV
        let funEditorPaned = FunEditorPaned edPaned
        
        gState <- newRef $ GState [] Nothing
        let gReader = GReader window 
                              funFunMenuBarST 
                              funToolbarST 
                              funMainPanedST
                              funInfoPanedST
                              funSymListST
                              funEditorPaned
                              funCommConsole
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
        
        onToolButtonClicked newFButton    (evalRWST createNewFile content st >> return ())
        onToolButtonClicked openFButton   (evalRWST openFile content st >> return ())
        onToolButtonClicked saveFButton   (evalRWST saveFile content st >> return ())
        onToolButtonClicked saveAtFButton (evalRWST saveAtFile content st >> return ())
        onToolButtonClicked closeFButton  (evalRWST closeCurrentFile content st >> return ())
        onToolButtonClicked checkMButton  (evalRWST checkSelectFile content st >> return ())
            
        return ()

configCommTV :: TextView -> IO ()
configCommTV commTV = do
        widgetModifyBase commTV StateNormal backColorCommTV
        widgetModifyText commTV StateNormal textColorCommTV
        widgetShowAll commTV

configCommandConsole :: GuiMonad ()
configCommandConsole= ask >>= \content ->
        io $ do
        let entry = content ^. (gFunCommConsole . commEntry)
        let buf = content ^. (gFunCommConsole . commTBuffer)
        let tv = content ^. (gFunCommConsole . commTView)
        entry `on` entryActivate $ io $ do
                        text <- entryGetText entry
                        entrySetText entry ""
                        titer <- textBufferGetEndIter buf
                        let str = "\n$ "++text
                        textBufferInsert buf titer str
                        res <- processCommand text
                        titer <- textBufferGetEndIter buf
                        textBufferInsert buf titer ("\n\t"++res)
                        titer2 <- textBufferGetEndIter buf
                        mark <- textBufferCreateMark buf Nothing titer2 False
                        textViewScrollToMark tv mark 0 Nothing
                        widgetShowAll tv
                        return ()
        return ()

processCommand :: String -> IO String
processCommand str = return "Procesamiento de comandos no implementado"                    

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
        set window [ windowDefaultWidth := 1000
                   , windowDefaultHeight := 600
                   ]

        widgetShowAll window
        onDestroy window mainQuit
        return ()
