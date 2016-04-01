{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
module GUI.GState where

import Fun.Environment
import Fun.Parser
import Fun.Eval.Eval(EvalEnv,createEvalEnv)

import GUI.EvalConsole.EvalComm

import qualified Equ.PreExpr as Equ

import Graphics.UI.Gtk hiding (get)

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.IORef
import Data.Reference
import qualified Data.Strict.Either as SEither

-- | Información sobre los items del menuBar.
data FunMenuBar = FunMenuBar { _quitButton :: MenuItem }
$(makeLenses ''FunMenuBar)

-- | Información sobre los items del toolBar.
data FunToolbar = FunToolbar { _symFrameCtrl :: (ToggleToolButton,CheckMenuItem)
                             , _axFrameCtrl  :: (ToggleToolButton,CheckMenuItem)
                             , _modFrameCtrl :: (ToggleToolButton,CheckMenuItem)
                             }
$(makeLenses ''FunToolbar)

data FunMainPaned = FunMainPaned { _mpaned :: HPaned }
$(makeLenses ''FunMainPaned)

-- Tipo para el resultado de una evaluación
type EvResult = SEither.Either String String

data FunCommConsole = FunCommConsole { _commEntry :: Entry
                                     , _commTBuffer :: TextBuffer
                                     , _commTView :: TextView
                                     }
$(makeLenses ''FunCommConsole)

data FunInfoConsole = FunInfoConsole { _infoConTBuffer :: TextBuffer
                                     , _infoConTView :: TextView
                                     }
$(makeLenses ''FunInfoConsole)

data FunEditorPaned = FunEditorPaned { _epaned :: VPaned }
$(makeLenses ''FunEditorPaned)

data FunInfoPaned = FunInfoPaned { _gDeclFrame    :: Frame 
                                 , _loadedMod :: Label
                                 }
$(makeLenses ''FunInfoPaned)

-- | Información sobre el panel derecho de la interfaz.
data FunEditBook = FunEditBook { _book        :: Notebook 
                               , _tabFileList :: [Maybe TextFilePath]
                               }
$(makeLenses ''FunEditBook)

-- | Información sobre la lista de símbolos.
data FunSymList = FunSymList { _gSymFrame    :: Frame
                             , _gSymIconView :: IconView
                             }
$(makeLenses ''FunSymList)

-- | Información sobre la lista de axiomas.
data FunAxList = FunAxList { _gAxFrame    :: Frame 
                           , _gAxTreeView :: TreeView
                           , _gAxRel      :: ComboBox
                           , _gAxLabelExpr :: Label
                           }
$(makeLenses ''FunAxList)

data FunEvalState = FunEvalState { -- expresión en el estado del evaluador:
                                   _evalExpr :: Maybe Equ.PreExpr
                                 , _evalEnv :: EvalEnv
                                 , _evalLComm :: Maybe EvalComm
                            }
$(makeLenses ''FunEvalState)


-- | Tipo de mónada de lectura. LLevamos toda la info necesaria recolectada
-- del archivo glade.
data GReader = GReader { _gFunWindow      :: Window
                       , _gFunToolbar     :: FunToolbar
                       , _gFunMainPaned   :: FunMainPaned
                       , _gFunInfoPaned   :: FunInfoPaned
                       , _gFunSymbolList  :: FunSymList
                       , _gFunAxiomList   :: FunAxList
                       , _gFunStatusbar   :: Statusbar
                       , _gFunEditorPaned :: FunEditorPaned
                       , _gFunCommConsole :: FunCommConsole
                       , _gFunInfoConsole :: FunInfoConsole
                       }
$(makeLenses ''GReader)

-- | Tipo de mónada de estado, llevamos el environment de un modulo bien 
-- chequeado y la info sobre la parte derecha de la interfaz, es decir, 
-- la que contiene los campos de texto para escribir programas.
data GState = GState { _gFunEnv :: Environment
                     , _gFunEditBook  :: FunEditBook
                     , _gFunEvalSt :: FunEvalState
                     }
$(makeLenses ''GState)

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
                
newEvalEnv :: GStateRef -> IO EvalEnv
newEvalEnv ref = readRef ref >>= \st ->
                 let funEnv = st ^. gFunEnv in
                    return (getFuncs funEnv) >>= \funcs ->
                    return (getVals funEnv) >>= \vals ->
                    return $ createEvalEnv funcs vals

initEvalEnv :: EvalEnv
initEvalEnv = createEvalEnv (getFuncs []) (getVals [])


updateEvalEnv :: GuiMonad ()
updateEvalEnv = getGState >>= \st ->
    let lcomm = st ^. (gFunEvalSt . evalLComm) in
        get >>= liftIO . newEvalEnv >>= \eEnv ->
        updateGState ((.~) gFunEvalSt (FunEvalState Nothing eEnv lcomm))
