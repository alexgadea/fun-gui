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
import Fun.Module.Error
import Fun.Declarations
import Fun.Derivation hiding (prog)
import Fun.Verification

import GUI.InfoConsole

data DeclState = DNoState
               | DUnknown
               | DChecked
               | DError

-- | Nombre a mostrar y acción al hace click.
data DeclItem = DeclItem { _declName  :: String
                         , _declPos   :: Maybe DeclPos
                         , _declErr   :: Maybe String
                         , _declState :: Maybe DeclState
                         }
$(mkLenses ''DeclItem)

type DeclType = String

type ForestDecl d = (DeclType,[(DeclPos,d)],[ErrInDecl d])

strToDeclItem :: String -> DeclItem
strToDeclItem s = DeclItem s Nothing Nothing Nothing

toForestDecl :: (Decl d, Show d) => ForestDecl d -> Forest DeclItem
toForestDecl (dt,ds,ids) = 
            [Node (strToDeclItem dt)
                  (map (\(pos,d) -> Node (newValidItem d (Just pos)) []) ds
                  ++
                  map (\errd -> 
                        Node (newInvalidItem (Just $ show $ errs errd)
                                             (eDecl errd) 
                                             (Just $ ePos errd)
                                             ) []) ids
                  )
            ] 

toForestVerif :: (String,[Verification],[ErrInVerif Verification]) -> 
                 Forest DeclItem
toForestVerif (dt,vs,ivs) = 
            [Node (strToDeclItem dt)
                  (map (\v-> Node (newValidItem (prog v) Nothing) []) vs
                  ++
                  map (\errd -> 
                        Node (newInvalidItem (Just $ show $ fst errd)
                                             (prog $ snd errd) 
                                             Nothing
                                             ) []) ivs
                  )
            ]


toForestDer :: (String,[(DeclPos,DerivDecl)],[ErrInDeriv DerivDecl]) -> 
               Forest DeclItem
toForestDer (dt,ds,ids) = 
            [Node (strToDeclItem dt)
                  (map (\(pos,d)-> Node (newValidItem d (Just pos)) []) ds
                  ++
                  map (\errd -> 
                        Node (newInvalidItem (Just $ show $ fst errd)
                                             (snd errd)
                                             Nothing
                                             ) []) ids
                  )
            ]

toForestEnv :: Environment -> Forest DeclItem
toForestEnv = map (\m -> Node (strToDeclItem $ unpack $ modName m) (newDecls m))
    where
        newDeclSpec :: Module -> Forest DeclItem
        newDeclSpec m = toForestDecl ( "Especificaciones"
                                     , specs $ validDecls m
                                     , inSpecs $ decls $ invalidDecls  m
                                     )
        newDeclFunc :: Module -> Forest DeclItem
        newDeclFunc m = toForestDecl ( "Functions"
                                     , functions $ validDecls m
                                     , inFunctions $ decls $ invalidDecls m
                                     )
        newDeclThm :: Module -> Forest DeclItem
        newDeclThm m = toForestDecl ( "Teoremas"
                                    , theorems $ validDecls m
                                    , inTheorems $ decls $ invalidDecls m
                                    )
        newDeclVal :: Module -> Forest DeclItem
        newDeclVal m = toForestDecl ( "Valores"
                                    , vals $ validDecls m
                                    , inVals $ decls $ invalidDecls m
                                    )
        newDeclProps :: Module -> Forest DeclItem
        newDeclProps m = toForestDecl ( "Proposiciones"
                                    , props $ validDecls m
                                    , inProps $ decls $ invalidDecls m
                                    )
                                    
                                    
        newDeclDer :: Module -> Forest DeclItem
        newDeclDer m = toForestDer  ( "Derivaciones"
                                    , derivs $ validDecls m
                                    , inDerivs $ decls $ invalidDecls m
                                    )
        newVerif :: Module -> Forest DeclItem
        newVerif m = toForestVerif ( "Verificaciones"
                                   , verifications m
                                   , verifs $ invalidDecls m
                                   )
        newDecls :: Module -> Forest DeclItem
        newDecls m = newDeclSpec m ++ newDeclFunc m ++ 
                     newDeclThm m  ++ newDeclVal m  ++
                     newDeclProps m ++
                     newDeclDer m ++ newVerif m

newValidItem :: Decl a => a -> Maybe DeclPos -> DeclItem
newValidItem   = newItem (Just DChecked) Nothing 
newInvalidItem :: Decl a => Maybe String -> a -> Maybe DeclPos -> DeclItem
newInvalidItem = newItem (Just DError) 

newItem :: Decl a => Maybe DeclState -> Maybe String -> a -> Maybe DeclPos -> DeclItem
newItem v err d p = DeclItem (unpack $ getNameDecl d) p err v

-- | Crea un treeStore para los DeclItem.
listDecls :: Environment -> GuiMonad (TreeStore DeclItem)
listDecls = io . treeStoreNew . toForestEnv

-- | Configura una lista de posición y declaracion.
setupDeclList :: Environment -> TreeView -> Window -> GuiMonad (TreeStore DeclItem)
setupDeclList env tv pwin  = listDecls (reverse env) >>= setupDList
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
                cellLayoutPackStart colName renderer False >>
                treeViewColumnSetSizing colName TreeViewColumnAutosize >>
                cellLayoutSetAttributes colName renderer list 
                                    (\ind -> [ cellText := ind ^. declName ]) >>
                                    
               cellRendererPixbufNew >>= \stateRend ->
               cellLayoutPackStart colSt stateRend False >>
               treeViewColumnSetSizing colSt TreeViewColumnAutosize >>
               cellLayoutSetAttributes colSt stateRend list 
                                    (\ind -> maybe [ cellPixbufStockId := stockDnd ]
                                            (\img -> [ cellPixbufStockId := img ])
                                            (declStateImg (ind ^. declState))) >>
                treeViewAppendColumn tv colSt >>
                treeViewAppendColumn tv colName >>
                treeViewGetSelection tv >>= \tree ->
                onSelectionChanged tree (evalRWST (onSelection list tree) content s >> return ()) >>
                return list    

declStateImg Nothing = Nothing
declStateImg (Just DNoState) = Nothing
declStateImg (Just DUnknown) = Just stockDialogQuestion
declStateImg (Just DChecked) = Just stockOk
declStateImg (Just DError) = Just stockDialogError

onSelection :: TreeStore DeclItem -> TreeSelection -> GuiMonad ()
onSelection list tree = do

    sel <- io $ treeSelectionGetSelectedRows tree
    
    unless (null sel) $ do
            let h = head sel
            mpos <- io $ (^. declPos) <$> treeStoreGetValue list h
            merr <- io $ (^. declErr) <$> treeStoreGetValue list h
            
            maybe (return ()) (printErrorMsg) merr
            
            case mpos of
                Nothing -> return ()
                Just pos -> do
                    s <- getGState
                    let Just ebook = s ^. gFunEditBook
                    let notebook = ebook ^. book
                    let mName = moduleName pos
                    
                    io $ containerForeach notebook
                            (\child -> notebookGetTabLabelText notebook child >>=
                            \(Just labtext) ->
                            (when (labtext == unpack mName) $
                                        selectPage notebook child >>
                                        getTextEditFromNotebook notebook >>= 
                                        \(_,tview) -> textViewGetBuffer tview >>= 
                                        \tbuffer -> selectText pos tbuffer tview >>
                                        treeSelectionUnselectAll tree >>
                                        return ()))

                
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
            
            updateInfo w content
            
            let labModule = content ^. (gFunInfoPaned . loadedMod)
            
            io $ setLoadedModuleInfo labModule $ maybe Nothing (Just . unpack) mname
            
            return ()
    where
        updateInfo :: Window -> GReader -> GuiMonad ()
        updateInfo w content = do
                    let declF = content ^. (gFunInfoPaned . gDeclFrame)
                    cs <- io $ containerGetChildren declF
                    tv <- io $ cleanTreeView $ castToAlignment (head cs)
                    setupDeclList env tv w
                    io $ widgetShowAll tv
                    return ()
        cleanTreeView :: Alignment-> IO TreeView
        cleanTreeView ali = containerGetChildren ali >>= \[tv] ->
                            containerRemove ali tv >>
                            treeViewNew >>= \tvnew ->
                            containerAdd ali tvnew >>
                            return tvnew
