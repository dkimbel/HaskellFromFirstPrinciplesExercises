{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- the below doesn't work, deliberately
--degenerateMtl :: Nat Maybe []
--degenerateMtl Nothing = []
--degenerateMtl (Just a) = [a + 1]

type BadNat f g a = f a -> g a

maybeToList' :: BadNat Maybe [] a
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

-- Without universal quantification to protect us, we can set
-- the Num typeclass constraint here and do (evil) addition
degenerateMtl' :: Num a => BadNat Maybe [] a
degenerateMtl' Nothing = []
degenerateMtl' (Just a) = [a + 1]

{- NOTES
   Natural transformation is basically the opposite of what
   functors are: with functors we leave outer strucutre alone
   while transforming the inner values, but with natural
   transformations, we alter the structure while leaving
   the values in place
-}
