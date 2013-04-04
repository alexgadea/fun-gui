module Main where

import Graphics.UI.Gtk hiding (get)

import GUI.Gui

main :: IO ()
main = do 
    initGUI
    
    xml <- builderNew
    builderAddFromFile xml "GUI/fun.ui"
    
    mainFunGui xml

    mainGUI
