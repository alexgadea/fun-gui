module GUI.InsertDialogs where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.RWS

import GUI.GState
import GUI.InfoConsole
import GUI.Utils
import GUI.EditBook

import Lens.Family

data DeclType = Spec | Fun | Val

data DeclDialog = DeclDialog { declType :: DeclType
                             , declName :: Entry
                             , declParams :: Entry
                             , declExpr :: TextView
                             , declDialog :: Dialog
}




createDeclDialog :: DeclType -> GuiMonad DeclDialog
createDeclDialog dtype = io $ do
    dlg <- dialogNew
    vbox <- dialogGetUpper dlg
    
    title <- labelTitle
    
    boxPackStart vbox title PackGrow 0
    
    table <- tableNew 3 2 False
    
    labelNFunc <- labelNew (Just "Nombre de Función:")
    labelParams <- labelNew (Just "Parámetros:")
    labelExpr <- labelNew (Just "Expresión:")
    
    entryNFunc <- entryNew
    entryParams <- entryNew
    tvExpr <- textViewNew
    
    tableAttachDefaults table labelNFunc 0 1 0 1
    tableAttachDefaults table labelParams 0 1 1 2
    tableAttachDefaults table labelExpr 0 1 2 3
    tableAttachDefaults table entryNFunc 1 2 0 1
    tableAttachDefaults table entryParams 1 2 1 2
    tableAttachDefaults table tvExpr 1 2 2 3
    
    boxPackStart vbox table PackGrow 0
    
    
    bok <- buttonNewFromStock stockOk
    bcancel <- buttonNewFromStock stockCancel
    
    dialogAddActionWidget dlg bok ResponseOk
    dialogAddActionWidget dlg bcancel ResponseCancel
    
    return DeclDialog { declType = dtype
                      , declName = entryNFunc
                      , declParams = entryParams
                      , declExpr = tvExpr
                      , declDialog = dlg
            }

    where labelTitle = labelNew $ Just $ case dtype of
                                            Spec -> "Insertar Especificación"
                                            Fun -> "Insertar Función"
                                            Val -> "Insertar Valor"
    
    
    
    
runDialog :: DeclDialog -> GuiMonad ()
runDialog decDlg = 
    getGState >>= \st -> ask >>= \content ->
    case st ^. gFunEditBook of
        Nothing -> printErrorMsg "No hay ningún archivo abierto"
        Just fbook -> getTextEditFromFunEditBook fbook >>= \(_,tv) ->
                      io $ do
                          let dlg = declDialog decDlg
                          setTitleDlg
                          vbox <- dialogGetUpper dlg
                          widgetShowAll vbox
                          response <- dialogRun dlg
                          case response of
                               ResponseOk -> getTextDecl >>= insertText tv
                               _ -> return ()
                          widgetDestroy dlg


    where getTextDecl :: IO String
          getTextDecl = do
            fname <- entryGetText $ declName decDlg
            params <- entryGetText $ declParams decDlg
            expr <- getTextExpr
            case declType decDlg of
                Spec -> return $ "let spec " ++ fname ++ " " ++ params ++ " = " ++ expr
                Fun -> return $ "let fun " ++ fname ++ " " ++ params ++ " = " ++ expr
                Val -> return $ "let val " ++ fname ++ " = " ++ expr
            
            
          getTextExpr = do
            exprBuf <- textViewGetBuffer $ declExpr decDlg
            iter1 <- textBufferGetStartIter exprBuf
            iter2 <- textBufferGetEndIter exprBuf
            expr <- textBufferGetText exprBuf iter1 iter2 False
            return expr
          
          insertText :: TextView -> String -> IO ()
          insertText tv str = do
              tbuf <- textViewGetBuffer tv
              textBufferInsertAtCursor tbuf ("\n"++str++"\n")
              
          setTitleDlg =  let dlg = declDialog decDlg in
                        case declType decDlg of
                            Spec -> windowSetTitle dlg "Insertar especificación"
                            Fun -> windowSetTitle dlg "Insertar función"
                            Val -> windowSetTitle dlg "Insertar valor"