module Utils where

maybeContains :: Eq a => a -> Maybe a -> Bool
maybeContains a m =
  case m of
    Just b -> a == b
    Nothing -> False
