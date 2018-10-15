incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just n) = Just $ show n
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = 
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope = LolNope

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right a) = Right $ a + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right a) = Right $ show a
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- We can't write a Functor instance to apply a value only
-- to First (rather than Second) because of the order of the
-- type arguments. When defining a typeclass instance, we
-- can't partially apply the second type argument but not
-- the first (if there were a syntax for that, it would
-- perhaps look like `instance Functor (Sum * a) where`
-- or `instance Functor (flip Sum a) where`, but `flip`
-- can't be used at the type level.
