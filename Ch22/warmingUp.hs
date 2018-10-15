import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char] 
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  a <- rev
  b <- cap
  return (a, b)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind =
  rev >>= (\x ->
    cap >>= (\y ->
      return (x, y)))
