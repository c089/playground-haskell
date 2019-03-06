module MyOptional(Optional(Nada, Only))  where

data Optional a = Nada | Only a deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend a b =
    case a of
      Nada -> b
      Only a' -> case b of
        Nada -> Only a'
        Only b' -> Only (mappend a' b')
