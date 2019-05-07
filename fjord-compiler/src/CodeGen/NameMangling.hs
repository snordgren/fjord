module CodeGen.NameMangling (
  mangle,
  mangleImport,
) where

import qualified Data.List as List


mangle :: String -> String
mangle name = 
  let 
    opFunNameChar c = 
      case c of 
        '+' -> "$plus"
        '-' -> "$minus"
        '/' -> "$div"
        '*' -> "$times"
        '^' -> "$up"
        ':' -> "$colon"
        '<' -> "$less"
        '>' -> "$more"
        '&' -> "$amp"
        '|' -> "$bar"
        '?' -> "$qmark"
        '@' -> "$at"
        '~' -> "$tilde"
        '=' -> "$eq"
        '%' -> "$pct"
        '!' -> "$bang"
        '(' -> []
        ')' -> []
        a -> a : []
  in 
    List.concat $ fmap opFunNameChar name
  

mangleImport :: String -> String
mangleImport s =
  let 
    mapF a = 
      case a of 
        '.' -> '_'
        _ -> a
  in
    fmap mapF s
