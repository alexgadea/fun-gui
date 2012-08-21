{-# Language DoAndIfThenElse, TemplateHaskell #-}
-- | Configuración de la lista de declaraciones del panel izquierdo.
module GUI.DeclList where

import GUI.GState
import GUI.EditBook
import GUI.Utils

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import Data.Map (empty,elems)
import Data.Tree
import Data.Tuple (swap)
import Text.Parsec.Pos (sourceLine,sourceColumn)

import qualified Data.Foldable as F (mapM_) 

import Control.Monad.Trans.RWS
import Control.Monad
import Control.Applicative

import Lens.Family
import Lens.Family.TH

import Fun.Decl
import Fun.Decl.Error
import Fun.Environment
import Fun.Module
import Fun.Declarations

import GUI.InfoConsole

data DeclState = DNoState
               | DUnknown
               | DChecked
               | DError

-- | Nombre a mostrar y acción al hace click.
data DeclItem = DeclItem { _declName :: String
                         , _declPos :: DeclPos
                         , _declState :: DeclState
                         }
$(mkLenses ''DeclItem)


-- | Crea un treeStore para los DeclItem.
listDecls :: (Decl d, Show d) => [(DeclPos,d)] -> GuiMonad (TreeStore DeclItem)
listDecls decls = io $ treeStoreNew $ toForest decls

-- | Crea un forest de DeclItem.
toForest :: (Decl d, Show d) => [(DeclPos,d)] -> Forest DeclItem
toForest = map (\(pos,d) -> Node (newItem d pos) [])
    where newItem d p = DeclItem (unpack $ getNameDecl d) p DNoState

-- | Configura una lista de posición y declaracion.
setupDeclList :: (Decl d, Show d) => [(DeclPos,d)] -> TreeView -> Window -> 
                                     GuiMonad (TreeStore DeclItem)
setupDeclList decls tv pwin  = listDecls decls >>= setupDList
    where
        setupDList :: TreeStore DeclItem -> GuiMonad (TreeStore DeclItem)
        setupDList list = do
            content <- ask
            s <- get
            io $               
                treeViewGetColumn tv 0 >>=                
                F.mapM_ (treeViewRemoveColumn tv) >>
                treeViewColumnNew >>= \colSt ->
                treeViewColumnNew >>= \colName ->
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list >>
                cellRendererTextNew >>= \renderer ->
                cellRendererPixbufNew >>= \stateRend ->
                cellLayoutPackStart colSt stateRend False >>
                cellLayoutPackStart colName renderer False >>
                cellLayoutSetAttributes colName renderer list 
                                    (\ind -> [ cellText := ind ^. declName ]) >>
                cellLayoutSetAttributes colSt stateRend list 
                                    (\ind -> maybe []
                                            (\img -> [ cellPixbufStockId := img ])
                                            (declStateImg (ind ^. declState))) >>
                treeViewAppendColumn tv colSt >>
                treeViewAppendColumn tv colName >>
                treeViewGetSelection tv >>= \tree ->
                onSelectionChanged tree (evalRWST (onSelection list tree) content s >> return ()) >>
                return list


declStateImg DNoState = Nothing
declStateImg DUnknown = Just stockDialogQuestion
declStateImg DChecked = Just stockOk
declStateImg DError = Just stockDialogError

onSelection :: TreeStore DeclItem -> TreeSelection -> GuiMonad ()
onSelection list tree = do

    sel <- io $ treeSelectionGetSelectedRows tree
    if null sel then return ()
        else do
            let h = head sel
            pos <- io $ (^. declPos) <$> treeStoreGetValue list h 
            
            s <- getGState
            let Just ebook = s ^. gFunEditBook
            let notebook = ebook ^. book
            let mName = moduleName pos
            
            io $ containerForeach notebook
                    (\child -> notebookGetTabLabelText notebook child >>=
                    \(Just labtext) ->
                    if labtext == unpack mName
                        then selectPage notebook child >>
                                getTextEditFromNotebook notebook >>= 
                                \(_,tview) -> textViewGetBuffer tview >>= 
                                \tbuffer -> selectText pos tbuffer tview >>
                                treeSelectionUnselectAll tree >>
                                return ()  
                        else return () )

                
    where showModNotLoaded mName = 
                putStrLn ("La declaración seleccionada está definida "++
                              "en el módulo " ++ unpack mName)
          selectPage notebook tab = 
              notebookPageNum notebook tab >>=
              \(Just nPage) -> notebookSetCurrentPage notebook nPage
                    
                    
selectText :: DeclPos -> TextBuffer -> TextView -> IO ()
selectText pos tbuf tview = do
    let initLine = sourceLine $ begin pos
    let endLine = sourceLine $ end pos
    
--     let initOffset = sourceColumn $ begin pos
--     let endOffset = sourceColumn $ end pos 
--     iter1 <- textBufferGetIterAtLineOffset tbuf (initLine-1) (initOffset-1)
--     iter2 <- textBufferGetIterAtLineOffset tbuf (endLine-1) (endOffset-1)
       
    iter1 <- textBufferGetIterAtLine tbuf (initLine-1)
    iter2 <- textBufferGetIterAtLine tbuf (endLine-1)

    -- scroll hasta la posicion donde termina la declaracion.
    mark <- textBufferCreateMark tbuf Nothing iter1 False
    textViewScrollToMark tview mark 0 Nothing
    
    textBufferSelectRange tbuf iter1 iter2
                    
-- | Configura las acciones de los DeclItem del panel izquierdo.
updateInfoPaned :: Environment -> Maybe ModName -> GuiMonad ()
updateInfoPaned env mname = do
            content <- ask 
            let w = content ^. gFunWindow
            let sb = content ^. gFunStatusbar
            let specsList   = concatMap (specs . decls) env
            let funcsList   = concatMap (functions . decls) env
            let thmsList    = concatMap (theorems . decls) env
            let valsList    = concatMap (vals . decls) env
            let propsList   = concatMap (props . decls) env

            updateInfo specsList iSpecs w content
            updateInfo funcsList iFuncs w content
            updateInfo thmsList  iThms  w content
            updateInfo valsList  iVals  w content
            updateInfo propsList iProps w content
            
            
            let labModule = content ^. (gFunInfoPaned . loadedMod)
            
            io $ setLoadedModuleInfo labModule $ maybe Nothing (Just . unpack) mname
            
            return ()
    where
        updateInfo [] getExpndr w content = do
                    let expander = content ^. (gFunInfoPaned . getExpndr)
                    io $ cleanExpander expander
                    io $ set expander [ expanderExpanded := False ]
        updateInfo decls getExpndr w content = do
                    let expander = content ^. (gFunInfoPaned . getExpndr)
                    tv <- io treeViewNew
                    setupDeclList decls tv w
                    io $ cleanExpander expander
                    io $ set expander [ containerChild := tv 
                                      , expanderExpanded := True
                                      ]
                    io $ widgetShowAll expander
                    return ()
        cleanExpander :: Expander -> IO ()
        cleanExpander e = do
                    cs <- io $ containerGetChildren e
                    when (length cs > 1) (containerRemove e $ head cs)
