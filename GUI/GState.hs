{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module GUI.GState where

import Lens.Family
import Lens.Family.TH

import Fun.Environment
import Fun.Module
import Fun.Eval.Interact(EvResult)

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Data.IORef
import Data.Reference

import Control.Concurrent
import Control.Concurrent.STM.TMVar

-- | Información sobre los items del menuBar.
data FunMenuBar = FunMenuBar { _quitButton :: MenuItem }
$(mkLenses ''FunMenuBar)

-- | Información sobre los items del toolBar.
data FunToolbar = FunToolbar { _symFrameB :: ToggleToolButton }
$(mkLenses ''FunToolbar)

data FunMainPaned = FunMainPaned { _mpaned :: HPaned }
$(mkLenses ''FunMainPaned)

data FunCommConsole = FunCommConsole {
                            _commEntry :: Entry
                          , _commTBuffer :: TextBuffer
                          , _commTView :: TextView
                          , _commChan :: TMVar String
                          , _commRepChan :: TMVar EvResult
                      }
$(mkLenses ''FunCommConsole)

data FunInfoConsole = FunInfoConsole {
                            _infoConTBuffer :: TextBuffer
                          , _infoConTView :: TextView
                      }
$(mkLenses ''FunInfoConsole)

data FunEditorPaned = FunEditorPaned { _epaned :: VPaned }
$(mkLenses ''FunEditorPaned)

data FunInfoPaned = FunInfoPaned { _iSpecs    :: Expander
                                 , _iFuncs    :: Expander
                                 , _iThms     :: Expander
                                 , _iVals     :: Expander
                                 , _iProps    :: Expander
                                 , _loadedMod :: Label
                                 }
$(mkLenses ''FunInfoPaned)

-- | Información sobre el panel derecho de la interfaz.
data FunEditBook = FunEditBook { _book :: Notebook }
$(mkLenses ''FunEditBook)

-- | Información sobre la lista de símbolos.
data FunSymList = FunSymList { _gSymFrame    :: Frame
                             , _gGoLeftBox   :: HBox
                             , _gScrollW     :: ScrolledWindow
                             , _gSymIconView :: IconView
                             , _gGoRightBox  :: HBox
                             }
$(mkLenses ''FunSymList)

-- | Tipo de mónada de lectura. LLevamos toda la info necesaria recolectada
-- del archivo glade.
data GReader = GReader { _gFunWindow    :: Window
                       , _gFunToolbar   :: FunToolbar
                       , _gFunMainPaned :: FunMainPaned
                       , _gFunInfoPaned :: FunInfoPaned
                       , _gFunSymbolList :: FunSymList
                       , _gFunStatusbar :: Statusbar
                       , _gFunEditorPaned :: FunEditorPaned
                       , _gFunCommConsole :: FunCommConsole
                       , _gFunInfoConsole :: FunInfoConsole
                       }
$(mkLenses ''GReader)

-- | Tipo de mónada de estado, llevamos el environment de un modulo bien 
-- chequeado y la info sobre la parte derecha de la interfaz, es decir, 
-- la que contiene los campos de texto para escribir programas.
data GState = GState { _gFunEnv :: Environment 
                     , _gFunEditBook  :: Maybe FunEditBook
                     }
$(mkLenses ''GState)

-- | Referencia del estado.
type GStateRef = IORef GState

-- | Mónada de la interfaz.
type GuiMonad' = RWST GReader () GStateRef 
type GuiMonad = GuiMonad' IO

instance Reference IORef GuiMonad where
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

-- | Retorna el estado de la mónada de la interfaz.
getGState :: GuiMonad GState
getGState = get >>= readRef

-- | Actualiza el estado de la mónada de la interfaz.
updateGState :: (GState -> GState) -> GuiMonad ()
updateGState f = do
                r <- get
                gst <- readRef r
                writeRef r $ f gst
                put r
