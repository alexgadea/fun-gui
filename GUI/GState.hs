{-# LANGUAGE TemplateHaskell #-}
module GUI.GState where

import Lens.Family
import Lens.Family.TH

import Fun.Environment

import Graphics.UI.Gtk

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

data FunMenuBar = FunMenuBar { _quitButton :: MenuItem }
$(mkLenses ''FunMenuBar)

data FunToolbar = FunToolbar { _openFB  :: ToolButton
                             , _checkMB :: ToolButton
                             }
$(mkLenses ''FunToolbar)

data FunPaned = FunPaned { _linesInfo :: Label
                         , _textLines :: TextView
                         }
$(mkLenses ''FunPaned)

data GReader = GReader { _gFunWindow   :: Window
                       , _gFunMenuBar  :: FunMenuBar
                       , _gFunToolbar  :: FunToolbar
                       , _gFunPaned    :: FunPaned
                       }
$(mkLenses ''GReader)

data GState = GState { _gFunEnv :: Environment }
$(mkLenses ''GState)

type GuiReader a = ReaderT GReader IO a
type GuiState a = StateT GState IO a

type GuiMonad a = RWST GReader () GState IO a

-- Estaria genial tener una función así, pero no andan los tipos :(
-- manyGet :: a1 -> [GetterFamily a1 a' a b'] -> [a]
-- manyGet content actions = foldr (\l r -> (content ^. l) : r) [] actions
