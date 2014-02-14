module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding (get)

import GUI.Gui

main :: IO ()
main = do 
    _ <- initGUI
    
    xml <- builderNew
    builderAddFromFile xml "GUI/fun.ui"
    
    args <- getArgs
    
    _ <- mainFunGui xml args
    
    mainGUI
