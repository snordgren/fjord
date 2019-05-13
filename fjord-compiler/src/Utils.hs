module Utils where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

maybeContains :: Eq a => a -> Maybe a -> Bool
maybeContains a m =
  case m of
    Just b -> a == b
    Nothing -> False


removeTopFolder :: String -> String
removeTopFolder a =
  let 
    firstFolderIx = 
      Maybe.fromMaybe 0 $ fmap ((+) 1) $ List.findIndex ((==) '/') a
  in
    List.drop firstFolderIx a
