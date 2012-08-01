module GUI.Config where

import Graphics.UI.Gtk


-- Para definir el lenguaje del resaltado

languageSpecFolder = "GUI/sourceview/language-specs"

languageSpecFunFile = languageSpecFolder ++ "/fun.lang"

anotherLanguage = languageSpecFolder ++ "/haskell.lang"

textStylesFolder = "GUI/sourceview/styles"

textStyleFile = textStylesFolder ++ "/fun.xml"

funMimeType = "text/fun"


-- Para el identado:

funIdentWidth :: Int
funIdentWidth = 4
spacesInsteadTab = True
setIndentOnTab = True
autoIdent = True


-- Colores

backColorCommTV :: Color
backColorCommTV = Color 8000 8000 8000

textColorCommTV :: Color
textColorCommTV = Color 60000 60000 60000

scrollInc :: Double
scrollInc = 10.0

scrollDec :: Double
scrollDec = - scrollInc
