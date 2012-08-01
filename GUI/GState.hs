{-# LANGUAGE TemplateHaskell,FlexibleInstances, TypeSynonymInstances,MultiParamTypeClasses,NoMonomorphismRestriction #-}
module GUI.GState where

import Lens.Family
import Lens.Family.TH

import Fun.Environment

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Data.IORef
import Data.Reference


io = liftIO

data FunMenuBar = FunMenuBar { _quitButton :: MenuItem }
$(mkLenses ''FunMenuBar)

data FunToolbar = FunToolbar { _newFB     :: ToolButton
                             , _openFB    :: ToolButton
                             , _saveFB    :: ToolButton
                             , _saveAtFB  :: ToolButton
                             , _closeFB   :: ToolButton
                             , _checkMB   :: ToolButton
                             }
$(mkLenses ''FunToolbar)

data FunPaned = FunPaned { _linesInfo :: Label
                         , _textLines :: TextView
                         }
$(mkLenses ''FunPaned)

data FunCommConsole = FunCommConsole {
                            _commEntry :: Entry
                          , _commTBuffer :: TextBuffer
                      }
$(mkLenses ''FunCommConsole)

data FunMainPaned = FunMainPaned { _mpaned :: HPaned }
$(mkLenses ''FunMainPaned)

data FunEditorPaned = FunEditorPaned { _epaned :: VPaned }
$(mkLenses ''FunEditorPaned)

data FunInfoPaned = FunInfoPaned { _iSpecs    :: Expander
                                 , _iFuncs    :: Expander
                                 , _iThms     :: Expander
                                 , _iVals     :: Expander
                                 , _iProps    :: Expander
                                 }
$(mkLenses ''FunInfoPaned)

data FunEditBook = FunEditBook { _book :: Notebook }
$(mkLenses ''FunEditBook)

data GReader = GReader { _gFunWindow    :: Window
                       , _gFunMenuBar   :: FunMenuBar
                       , _gFunToolbar   :: FunToolbar
                       , _gFunMainPaned :: FunMainPaned
                       , _gFunInfoPaned :: FunInfoPaned
                       , _gFunEditorPaned :: FunEditorPaned
                       , _gFunCommConsole :: FunCommConsole
                       }
$(mkLenses ''GReader)

data GState = GState { _gFunEnv :: Environment 
                     , _gFunEditBook  :: Maybe FunEditBook
                     }
$(mkLenses ''GState)

type GStateRef = IORef GState

type GuiMonad = RWST GReader () GStateRef IO

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

getGState :: GuiMonad GState
getGState = get >>= readRef

updateGState :: (GState -> GState) -> GuiMonad ()
updateGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r

-- Estaria genial tener una función así, pero no andan los tipos :(
-- manyGet :: a1 -> [GetterFamily a1 a' a b'] -> [a]
-- manyGet content actions = foldr (\l r -> (content ^. l) : r) [] actions
