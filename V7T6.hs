{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE Rank2Types#-}

kissa :: Functor f => f (f a -> a) -> f a
kissa fp = fmap () fp

-- ..................

{-newtype Kissa f a = Kissa (f a -> a)
instance (Functor f) => Functor (Kissa f) where
  fmap :: Functor f => (a->b) -> Kissa f a -> Kissa f b
  fmap f (Kissa fa) = Kissa p where
    p :: Functor f => f b -> b
    p fb = fa (f . fb)-}
    -- fmap f fb(a->b)
    --fa :: f (a->b) -> a
    --f :: a -> b
    --p :: f b -> b  
    --fa :: f a -> a
    --fb :: f b
