module GUI.EditBook where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Arrow

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust)

import GUI.GState


configTextView :: Label -> TextBuffer -> GuiMonad ()
configTextView linesI buf = liftIO $ do
        onBufferChanged buf ( do
            countlLine <- textBufferGetLineCount buf
            let iplusone = unlines [show i | i <- [1..countlLine]]
            labelSetText linesI iplusone )
        return ()

configInfoLines :: Label -> GuiMonad ()
configInfoLines l = io $ do 
            set l [ miscXalign := 0
                  , miscYalign := 0
                  , miscXpad   := 2
                  ]
    
configScrolledWindow :: ScrolledWindow -> GuiMonad ()
configScrolledWindow sw = io $ do
            set sw [ scrolledWindowHscrollbarPolicy := PolicyAutomatic 
                   , scrolledWindowVscrollbarPolicy := PolicyAutomatic 
                   ]
                   
configNotebook :: Notebook -> GuiMonad ()
configNotebook nb = io $ do
            set nb [ notebookTabBorder  := 0
                   , notebookTabHborder := 0
                   , notebookTabVborder := 0
                   ]

createTextEntry :: Maybe String -> GuiMonad HBox
createTextEntry mcode = do
            hbox <- io $ hBoxNew False 0
            
            l <- io $ labelNew $ Just "1"
            configInfoLines l
            
            buf <- io $ textBufferNew Nothing
            
            configTextView l buf
            
            maybe (return ()) (io . loadCode buf) mcode
            
            textv <- io $ textViewNewWithBuffer buf
            
            io $ boxPackStart hbox l PackNatural 0
            io $ boxPackStart hbox textv PackGrow 0
            
            return hbox
    where
        loadCode :: TextBuffer -> String -> IO ()
        loadCode buf code = do
                start <- textBufferGetStartIter buf
                textBufferInsert buf start code

createTextEdit :: Maybe String -> GuiMonad ScrolledWindow
createTextEdit mcode = do
            swindow <- io $ scrolledWindowNew Nothing Nothing
            configScrolledWindow swindow
            
            hAdj <- io $ adjustmentNew 0 0 100 1 10 10
            vAdj <- io $ adjustmentNew 0 0 100 1 10 10
            
            texte <- createTextEntry mcode
            
            portv <- io $ viewportNew hAdj vAdj
            io $ set portv   [containerChild := texte]
            
            io $ widgetShowAll texte
            
            io $ widgetShowAll portv
            
            io $ set swindow [containerChild := portv]
            
            io $ widgetShowAll swindow
            
            return swindow

getTextEditFromFunEditBook :: FunEditBook -> GuiMonad (String,TextView)
getTextEditFromFunEditBook feditBook = do
            let notebook = feditBook ^. book
            cPageNum       <- io $ notebookGetCurrentPage notebook
            Just cpSW      <- io $ notebookGetNthPage notebook cPageNum
            Just textViewN <- io $ notebookGetTabLabelText notebook cpSW
            [cpPV]    <- io $  containerGetChildren (castToContainer cpSW)
            [cpBox]   <- io $ containerGetChildren (castToContainer cpPV)
            [_,tv]    <- io $ containerGetChildren (castToContainer cpBox)
            return $ (textViewN,castToTextView tv)

createEditBook :: Maybe String -> Maybe String -> GuiMonad Notebook
createEditBook mname mcode = do
            let name = maybe "blank" id mname
            
            newnt <- io (notebookNew)
            configNotebook newnt
            
            texte <- createTextEdit mcode
            
            io $ notebookAppendPage newnt texte name
            
            content <- ask 
            
            let mainPaned = content ^. (gFunMainPaned . mpaned)
            
            io $ panedAdd2 mainPaned newnt
            io $ widgetShowAll mainPaned
            
            return newnt
