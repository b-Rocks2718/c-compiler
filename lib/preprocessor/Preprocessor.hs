module Preprocessor(preprocess) where

import Utils ( (=~) )

-- takes a regex to match comments and the string to process
removeComments :: String -> String -> String
removeComments regex s
  | commentMatch = preprocess (a ++ c)
  | otherwise = s
  where
    commentMatch = (s =~ regex) :: Bool
    (a, _, c) = (s =~ regex) :: (String, String, String)

-- regex library is in multiline mode
-- I coudln't figure out how to fix it
-- so newlines are removed here

-- inline comments (//) are removed first while newlines are still there
-- directives beginning with # are ignored for now
-- then newlines are removed
-- then multi line (/* */) comments can be removed
preprocess :: String -> String
preprocess = removeComments "/\\*([^*]|\\*+[^/])*\\*+/" .
             unwords . lines . removeComments "//.*$" . removeComments "#.*$"