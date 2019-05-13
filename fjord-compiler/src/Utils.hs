module Utils where

import Debug.Trace
import qualified Data.List as List
import qualified Data.List.Utils as List.Utils
import qualified Data.Maybe as Maybe

maybeContains :: Eq a => a -> Maybe a -> Bool
maybeContains a m =
  case m of
    Just b -> a == b
    Nothing -> False


removeTopFolder :: String -> String
removeTopFolder a =
  let 
    firstFolderIxTarg =
      if List.isPrefixOf "./" a then
        drop (List.length "./") a
      else
        a

    firstFolderIx =
      Maybe.fromMaybe 0 $ fmap ((+) 1) $ List.findIndex ((==) '/') firstFolderIxTarg
  in
      List.drop firstFolderIx firstFolderIxTarg


{- Recursively replace one list with another. -}
replaceRec :: Eq a => [a] -> [a] -> [a] -> [a]
replaceRec from to a =
  if List.isInfixOf from a then
    replaceRec from to $ List.Utils.replace from to a
  else
    a
