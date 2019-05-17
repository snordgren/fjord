module Check.Types.UseCounter where

data UseCounter
  = UseCounter {
    name :: !String,
    isUsedLinearly :: !Bool
  }


for :: String -> UseCounter
for name =
  UseCounter name False


markUsedLinearly :: UseCounter -> UseCounter
markUsedLinearly a =
  a { isUsedLinearly = True }
