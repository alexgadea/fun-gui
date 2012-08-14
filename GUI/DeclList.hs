{-# Language TypeOperators, RankNTypes, DoAndIfThenElse #-}
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

import Lens.Family

import Fun.Decl
import Fun.Decl.Error
import Fun.Environment
import Fun.Module
import Fun.Declarations

import GUI.InfoConsole

-- | Nombre a mostrar y acción al hace click.
type DeclItem = (String, DeclPos)

-- | Crea un treeStore para los DeclItem.
listDecls :: (Decl d, Show d) => [(DeclPos,d)] -> GuiMonad (TreeStore DeclItem)
listDecls decls = io $ treeStoreNew $ toForest decls

-- | Crea un forest de DeclItem.
toForest :: (Decl d, Show d) => [(DeclPos,d)] -> Forest DeclItem
toForest = map (\(pos,d) -> Node (unpack $ getNameDecl d, pos) [])

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
                treeViewColumnNew >>= \col ->
                treeViewSetHeadersVisible tv False >>
                treeViewSetModel tv list >>
                cellRendererTextNew >>= \renderer ->
                cellLayoutPackStart col renderer False >>
                cellLayoutSetAttributes col renderer list 
                                    (\ind -> [ cellText := fst ind ]) >>
                treeViewAppendColumn tv col >>
                treeViewGetSelection tv >>= \tree ->
                onSelectionChanged tree (evalRWST (onSelection list tree) content s >> return ()) >>
                return list

onSelection :: TreeStore DeclItem -> TreeSelection -> GuiMonad ()
onSelection list tree = do

    sel <- io $ treeSelectionGetSelectedRows tree
    if null sel then return ()
        else do
            let h = head sel
            (_,pos) <- io $ treeStoreGetValue list h
            
            s <- getGState
            let Just ebook = s ^. gFunEditBook
            let notebook = ebook ^. book
            let infoModules = ebook ^. modules
            let mName = moduleName pos
            let mntab = lookup mName (map swap infoModules)
            maybe (printInfoMsg ("La declaración seleccionada está definida "++
                                "en el módulo " ++ unpack mName) >> return ())
                (\ntab -> do
                    io $ notebookSetCurrentPage notebook ntab
                    (_,tview) <- getTextEditFromFunEditBook ebook
                    tbuffer <- io $ textViewGetBuffer tview
                    
                    io $ selectText pos tbuffer tview infoModules
                    io $ treeSelectionUnselectAll tree
                    return ()
                ) mntab
                    
selectText :: DeclPos -> TextBuffer -> TextView -> ModGui -> IO ()
selectText pos tbuf tview infoModules = do
    let initLine = sourceLine $ begin pos
    let endLine = sourceLine $ end pos
    
--     let initOffset = sourceColumn $ begin pos
--     let endOffset = sourceColumn $ end pos 
--     iter1 <- textBufferGetIterAtLineOffset tbuf (initLine-1) (initOffset-1)
--     iter2 <- textBufferGetIterAtLineOffset tbuf (endLine-1) (endOffset-1)
       
    iter1 <- textBufferGetIterAtLine tbuf (initLine-1)
    iter2 <- textBufferGetIterAtLine tbuf (endLine-1)

    
    textBufferSelectRange tbuf iter1 iter2
                    
-- | Configura las acciones de los DeclItem del panel izquierdo.
updateInfoPaned :: Environment -> Maybe ModName -> GuiMonad ()
updateInfoPaned env mname = do
            content <- ask 
            let w = content ^. gFunWindow
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
            
            maybe (io (labelSetText labModule "Ninguno")) 
                  (\name -> io (labelSetText labModule $ unpack name))
                  mname
            
            return ()
    where
--         updateInfo :: (Decl d, Show d) => [(DeclPos, d)] -> 
--                       ((Expander -> Lens.Family.Getting Expander b') -> 
--                         FunInfoPaned -> 
--                         Lens.Family.Getting Expander FunInfoPaned) -> 
--                       Window -> GReader -> GuiMonad ()
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
