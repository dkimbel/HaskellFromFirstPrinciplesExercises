returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

--returnBroke :: (((a -> b) -> c) -> d) -> d
--returnBroke _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a
