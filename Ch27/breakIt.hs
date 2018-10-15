x = undefined
y = x `seq` "blah"

main :: IO ()
main = do
  print (snd (x, y))
