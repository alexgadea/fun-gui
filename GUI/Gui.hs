-- | Interfaz gráfica de Equ.
module Main where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Arrow

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust)

import GUI.GState

import Fun.Environment
import Fun.Parser.Module

main :: IO ()
main = do 
    initGUI
    
    (greader,gstate) <- makeGState "GUI/fun.glade"
    
    runRWST (do configWindow
                configToolBarButtons
                configMenuBarButtons
                configTextView
            ) greader gstate

    mainGUI

makeGState :: String -> IO (GReader,GState) 
makeGState sXml = do
        Just xml <- xmlNew sXml
        
        lI <- xmlGetWidget xml castToLabel "linesInfo"
        tV <- xmlGetWidget xml castToTextView "textView"
        
        openFB <- xmlGetWidget xml castToToolButton "openFileButton"
        checkMB <- xmlGetWidget xml castToToolButton "checkModuleButton"
        
        window <- xmlGetWidget xml castToWindow "mainWindow"
        quitButton <- xmlGetWidget xml castToMenuItem "quitButton"
        
        let funPanedST = FunPaned lI tV
        let funToolbarST = FunToolbar openFB checkMB
        let funFunMenuBarST = FunMenuBar quitButton
        
        return $ (GReader window funFunMenuBarST funToolbarST funPanedST,GState [])

configMenuBarButtons :: GuiMonad ()
configMenuBarButtons = ask >>= \content -> get >>= \st ->
        liftIO $ do
        let checkMButton = content ^. (gFunToolbar . checkMB)
        
        onToolButtonClicked checkMButton (do
            let textV = content ^. (gFunPaned . textLines)
            buf <- textViewGetBuffer textV
            start <- textBufferGetStartIter buf
            end <- textBufferGetEndIter buf
            code <- textBufferGetText buf start end False
            eRes <- loadMainModuleFromString code
            either (putStrLn . show) (putStrLn . show) eRes)
            
        return ()

configToolBarButtons :: GuiMonad ()
configToolBarButtons = ask >>= \content ->
        liftIO $ do
        let (quitB,window) = flip (^.) (gFunMenuBar . quitButton) 
                             &&& 
                             flip (^.) (gFunWindow) $ content
        onActivateLeaf quitB $ widgetDestroy window
        return ()

configTextView :: GuiMonad ()
configTextView = ask >>= \content ->
        liftIO $ do
        let (textView,linesI) = flip (^.) (gFunPaned . textLines) 
                                &&& 
                                flip (^.) (gFunPaned . linesInfo) $ content
        buf <- textViewGetBuffer textView
        onBufferChanged buf ( do
            countlLine <- textBufferGetLineCount buf
            let iplusone = unlines [show i | i <- [1..countlLine]]
            labelSetText linesI iplusone )
        return ()

configWindow :: GuiMonad ()
configWindow = ask >>= \content -> 
        liftIO $ do
        let window = content ^. gFunWindow
        set window [ windowDefaultWidth := 600
                   , windowDefaultHeight := 600
                   ]

        widgetShowAll window
        onDestroy window mainQuit
        return ()
