module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp searchChar list
  = [y | (x,y) <- list, x== searchChar]


splitText :: [Char] -> String -> (String, [String])
splitText separators ""
  = ("", [""])
splitText separators sentence
  = (separators', sentence' separators sentence)
  where
    separators' = [c | c <- sentence,  elem c separators]

    sentence' :: [Char] -> String -> [String]
    sentence' separators ""
      = [""]
    sentence' separators (c:cs)
        | elem c separators = "" : listOfWords
        | otherwise         = (c:w) : ws
        where
          listOfWords@(w:ws)=  sentence' separators cs



combine :: String -> [String] -> [String]
combine "" sentence
  = sentence
combine (sep1:remSeps) (word1:remWords)
  = word1 : [sep1] : combine remSeps remWords

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []
  = []
getKeywordDefs (string1 : remStrings)
  = getKeywordDefs' string1 ++ getKeywordDefs remStrings
  where
    getKeywordDefs' :: String -> KeywordDefs
    getKeywordDefs' string
      = [(word1, concat (combine separators remWords))]
      where
        ((_ : separators),(word1 : remWords)) = splitText " " string

expand :: FileContents -> FileContents -> FileContents
expand sentence fillWords
  = concat (combine sep modifiedWords)
  where
    (sep, splitSentence) = splitText separators sentence
    (_,  splitFillWords) = splitText "\n" fillWords
    keywordsDef = getKeywordDefs splitFillWords
    modifiedWords = replaceWord splitSentence keywordsDef

replaceWord :: [String] -> KeywordDefs -> [String]
replaceWord [] _
 = []
replaceWord ("" : ws) defs
  = "": replaceWord ws defs
replaceWord (w@(c:cs) : ws) defs
    | c == '$'  = head (lookUp w defs) : replaceWord ws defs
    | otherwise = w : replaceWord ws defs

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
