module GUI.Config where




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