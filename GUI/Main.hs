module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding (get)
import System.Glib

import GUI.Gui
import Paths_fun_gui

main :: IO ()
main = do 
    _ <- initGUI
    
    _ <- setProgramName "fun"
    _ <- setApplicationName "fun"
    uifn <- getDataFileName "fun.ui"
    
    xml <- builderNew
    builderAddFromFile xml uifn
    
    args <- getArgs
    
    _ <- mainFunGui xml args
    
    mainGUI
