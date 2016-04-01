-- | Interfaz gráfica de Fun.
module GUI.Gui where

import Graphics.UI.Gtk hiding (get)
import qualified Graphics.UI.Gtk as G

import Control.Lens
import Control.Monad
import Control.Monad.Trans.RWS

import Data.Text (pack)
import Data.Reference
import Data.Maybe

import GUI.File
import GUI.GState
import GUI.SymbolList
import GUI.AxiomList
import GUI.EvalConsole
import GUI.InfoConsole
import GUI.Utils
import GUI.DeclList

onActivate' ::  MenuItemClass self => self -> IO () -> IO ()
onActivate' menu action = on menu menuItemActivated action >> return ()

mainFunGui :: Builder -> [FilePath] -> IO ()
mainFunGui xml fps = do 
                (gReader,gState) <- makeGState xml

                (_,_,_) <- runRWST (do  configWindow
                                        configMenuBarButtons  xml
                                        configToolBarButtons  xml
                                        configCommandConsole
                                        configSymbolList
                                        configAxiomList
                                        mapM_ (openFileFromPath . pack) fps
                                        ) gReader gState
                return ()

-- | Genera el estado inicial de la mónada.
makeGState :: Builder -> IO (GReader,GStateRef) 
makeGState xml = do
        
        
        mainPaned <- builderGetObject xml castToHPaned "mainPaned"
        
        declFrame  <- builderGetObject xml castToFrame "declFrame"
        loadedModule  <- builderGetObject xml castToLabel "labelLoadedModule"
        
        symFrame   <- builderGetObject xml castToFrame "symFrame"
        symIV      <- builderGetObject xml castToIconView "symbolList"
        
        axFrame  <- builderGetObject xml castToFrame "axiomFrame"
        axTV     <- builderGetObject xml castToTreeView "axiomList"
        axRel    <- builderGetObject xml castToComboBox "comboAxioms"
        axLabExpr <- builderGetObject xml castToLabel "axiomExpr"
        
        evalLbl <- builderGetObject xml castToLabel "evalLbl"
        infoLbl <- builderGetObject xml castToLabel "infoLbl"
        modulesLbl <- builderGetObject xml castToLabel "modulesLbl"
        
        window <- builderGetObject xml castToWindow "mainWindow"
        
        edPaned <- builderGetObject xml castToVPaned "editorPaned"
        commTV <- builderGetObject xml castToTextView "commandTView"
        commandEntry <- builderGetObject xml castToEntry "commandEntry"
        funStatusbar <- builderGetObject xml castToStatusbar "statusBar"
        infoTV <- builderGetObject xml castToTextView "infoConsoleTView"
        infoEvalNB <- builderGetObject xml castToNotebook "infoEvalNB"
        editModulesNB <- builderGetObject xml castToNotebook "editModulesNB"

        symFButton    <- builderGetObject xml castToToggleToolButton "symFrameButton"
        symFBItem     <- builderGetObject xml castToCheckMenuItem "symLabel"
        axFButton     <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
        axFBItem      <- builderGetObject xml castToCheckMenuItem "axLabel"
        modListButton <- builderGetObject xml castToToggleToolButton "modulesFrameButton"
        modListItem   <- builderGetObject xml castToCheckMenuItem "modItemMenu"

                         
        commTBuf <- textViewGetBuffer commTV
        
        infoTBuf <- textViewGetBuffer infoTV
        
        configInfoConsoleTV infoTV infoTBuf


        void $ evalLbl `on` labelActiveCurrentLink $ do
                       notebookSetCurrentPage infoEvalNB 1
                       widgetGrabFocus commandEntry
          
        void $ infoLbl `on` labelActiveCurrentLink $ do
                       notebookSetCurrentPage infoEvalNB 0
                       widgetGrabFocus infoTV

        void $ modulesLbl `on` labelActiveCurrentLink $ do
                          tv <- io $ getModulesTV declFrame
                          widgetGrabFocus tv

        void $ editModulesNB `on` focusInEvent $ io $ do
                             np <- G.get editModulesNB notebookPage
                             page <- notebookGetNthPage editModulesNB np
                             when (isJust page) $ do 
                                  let Just p = page
                                  let sw = castToScrolledWindow p
                                  chs <- containerGetChildren sw
                                  widgetGrabFocus (chs !! 0)
                             return True

        let funToolbarST    = FunToolbar (symFButton,symFBItem)
                                         (axFButton,axFBItem)
                                         (modListButton,modListItem)

        let funMainPanedST  = FunMainPaned mainPaned
        let funInfoPanedST  = FunInfoPaned declFrame loadedModule
        let funSymListST    = FunSymList symFrame symIV 
        let funAxListST     = FunAxList axFrame axTV axRel axLabExpr
        let funCommConsole  = FunCommConsole commandEntry commTBuf commTV
        let funInfoConsole  = FunInfoConsole infoTBuf infoTV
        let funEditorPaned  = FunEditorPaned edPaned
        let funEditBook     = FunEditBook editModulesNB []
        
        gState <- newRef $ GState []
                                  funEditBook
                                  (FunEvalState Nothing initEvalEnv Nothing)
                                  Nothing
        let gReader = GReader window 
                              funToolbarST
                              funMainPanedST
                              funInfoPanedST
                              funSymListST
                              funAxListST
                              funStatusbar 
                              funEditorPaned
                              funCommConsole
                              funInfoConsole
        return (gReader,gState)

-- | Configura los botones de la barra, tales como abrir, cerrar, etc...
configMenuBarButtons :: Builder -> GuiMonad ()
configMenuBarButtons xml = ask >>= \content -> get >>= \st -> io $ do

    newFButton    <- builderGetObject xml castToToolButton "newFileButton"
    openFButton   <- builderGetObject xml castToToolButton "openFileButton"
    saveFButton   <- builderGetObject xml castToToolButton "saveFileButton"
    saveAtFButton <- builderGetObject xml castToToolButton "saveFileAtButton"
    closeFButton  <- builderGetObject xml castToToolButton "closeFileButton"
    checkMButton  <- builderGetObject xml castToToolButton "checkModuleButton"
    symFButton    <- builderGetObject xml castToToggleToolButton "symFrameButton"
    symFBItem     <- builderGetObject xml castToCheckMenuItem "symLabel"
    axFButton     <- builderGetObject xml castToToggleToolButton "AxiomFrameButton"
    axFBItem      <- builderGetObject xml castToCheckMenuItem "axLabel"
    modListButton <- builderGetObject xml castToToggleToolButton "modulesFrameButton"
    modListItem   <- builderGetObject xml castToCheckMenuItem "modItemMenu"
      
    _ <- onToolButtonClicked newFButton    $ eval createNewFile content st
    _ <- onToolButtonClicked openFButton   $ eval openFile content st
    _ <- onToolButtonClicked saveFButton   $ eval saveFile content st
    _ <- onToolButtonClicked saveAtFButton $ eval saveAtFile content st
    _ <- onToolButtonClicked closeFButton  $ eval closeCurrentFile content st
    _ <- onToolButtonClicked checkMButton  $ eval checkSelectFile content st
    _ <- onToolButtonClicked symFButton    $ eval configSymFrameButton content st
    _ <- onToolButtonClicked axFButton $ eval axListToggle content st
    _ <- axFBItem `on` checkMenuItemToggled $ eval axListToggle content st
    _ <- symFBItem `on` checkMenuItemToggled $ eval configSymFrameButton content st
    _ <- onToolButtonClicked modListButton $ eval declListToggle content st
    _ <- modListItem `on` checkMenuItemToggled $ eval declListToggle content st
         
    return ()

-- | Configura los items del menu.
configToolBarButtons :: Builder -> GuiMonad ()
configToolBarButtons xml = ask >>= \content -> get >>= \st ->
            io $ do
            let window = content ^. gFunWindow

            -- Menu File
            newB  <- builderGetObject xml castToMenuItem "newButton"
            openB <- builderGetObject xml castToMenuItem "openButton"
            saveB  <- builderGetObject xml castToMenuItem "saveButton"
            saveAsB <- builderGetObject xml castToMenuItem "saveAsButton"
            closeB  <- builderGetObject xml castToMenuItem "closeButton"
            quitB  <- builderGetObject xml castToMenuItem "quitButton"

            _ <- onActivate' newB    $ eval createNewFile    content st
            _ <- onActivate' openB   $ eval openFile         content st
            _ <- onActivate' saveB   $ eval saveFile         content st
            _ <- onActivate' saveAsB $ eval saveAtFile       content st
            _ <- onActivate' closeB  $ eval closeCurrentFile content st
            _ <- onActivate' quitB   $ widgetDestroy window

            -- Menu Edición
            copyItem   <-  builderGetObject xml castToImageMenuItem "menuCopy"
            cutItem    <-  builderGetObject xml castToImageMenuItem "menuCut"
            pasteItem  <-  builderGetObject xml castToImageMenuItem "menuPaste"
            deleteItem <-  builderGetObject xml castToImageMenuItem "menuDelete"
            checkItem  <- builderGetObject xml castToMenuItem "checkButton"
            
            _ <- onActivate' checkItem  $ eval checkSelectFile  content st
            
            return ()

-- | Configura la ventana principal.
configWindow :: GuiMonad ()
configWindow = ask >>= \content -> get >>= \st -> 
            io $ do
            let window = content ^. gFunWindow
            let mainPaned = content ^. (gFunMainPaned . mpaned)
            windowMaximize window
            widgetShowAll window
            eval declListToggle content st
            _ <- window `on` unrealize $ mainQuit
            return ()

-- | Mensaje de error en caso de no encontrar el archivo glade correspondiente.
msgErrGladeNotFound :: String
msgErrGladeNotFound = "Archivo fun.glade no encontrado"
