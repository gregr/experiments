{-# LANGUAGE RankNTypes #-}

fa :: ((forall a. a -> a) -> Int -> Int) -> Int
fa = undefined

ga :: (Int -> Int) -> Int -> Int
ga = undefined

fb :: (forall a. a -> a) -> Int -> Int
fb = undefined

gb :: Int -> Int
gb = undefined

idid :: (forall a. a -> a) -> (forall a. a -> a)
idid x = x

pp = ((), id) :: ((), forall a. a -> a)

ppf pair = snd pair 4
