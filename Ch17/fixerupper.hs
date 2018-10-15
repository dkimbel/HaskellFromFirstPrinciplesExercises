fixerUpper1 = const <$> Just "Hello" <*> pure "World"

fixerUpper2 =
  (,,,) <$> Just 90 
        <*> Just 10 
        <*> Just "Tierness" 
        <*> pure [1,2,3]
