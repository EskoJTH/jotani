

{-#LANGUAGE DeriveFunctor#-}
module V6T1 where
{-Implement the following functions with traverse f, where traverse is from Data.Traversable and f is your own creation.

    tminimum :: (Traversable t, Bounded a, Ord a) => t a -> a to find the minimal element
    tmap :: Traversable t => (a -> b) -> t a -> t b to consistently change all elements
    tmodify :: Traversable t => (a -> a) -> t a -> t a to change the last element if it exists
-}

import Data.Traversable
import Data.Functor.Const
import Data.Semigroup
import Data.Foldable
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Control.Applicative.Backwards

--let a;
--for (let i of lista) if (i<a) a=i; 

--data Min a = Min a deriving (Functor, Show)
{-
instance (Ord a) => Semigroup (Min a) where
  (<>) (Min a) (Min b) = Min (min a b)

instance Applicative Min where
  pure = Min
  (<*>) (Min f) (Min b) = Min(f b)
-}

tminimum :: (Traversable t, Bounded a, Ord a) => t a -> a
tminimum list = getMin $ getConst $ traverse fun list where
  fun a = Const (Min a)
-- (Const . Min)


--Apujuttuja.
runMinList ::(Traversable t,Bounded a, Ord a) => Min (t a) -> a
runMinList (Min list) =(\(Min a) -> a) $ foldMap (\a->Min a) list
tminimum'' :: (Traversable t, Bounded a, Ord a) => t a -> a
tminimum'' list = runMinList $ traverse f list where
  f = (\a->Min a)
tminimum' ::(Bounded a, Ord a )=> [a] -> a
tminimum' list  = (\(Min a)->a) $ foldMap (\a->Min a) list
--traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--foldMap :: Monoid m => (a -> m) -> t a -> m
foldMaps :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMaps f = getConst . traverse fun where
  fun a = Const (f a)
sequenceA :: [Min a] -> Min [a]
sequenceA list = Min $ map (\(Min a) -> a) list
sequenceAC :: Monoid c => [Const c a] -> Const c [a]
sequenceAC [] = Const mempty
sequenceAC (x:xs) = Const $ getConst x `mappend` ys
  where ys = getConst $ sequenceAC xs
--Apujuttuja.

--consistently change all elements
tmap :: Traversable t => (a -> b) -> t a -> t b
tmap f list = runIdentity $ traverse (Identity . f) list

sequenceAT :: [Identity a] -> Identity [a]
sequenceAT [] = Identity mempty
sequenceAT (x:xs) =Identity $ runIdentity x : (runIdentity $ sequenceAT xs)

--fmap :: (a -> b) -> t a -> t b
--traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--(>>=)  :: m a -> (a -> m b) -> m b
-- to change the last element if it exists

tmodify :: Traversable t => (a -> a) -> t a -> t a
tmodify f list = _1 traverse State(Bool) list where
h x = _

{-

flip evalState 0 $

h a = StateT (\s-> p s)
  
  p s' = case s' of
    () -> (a, 1)
    _ -> (a, 1)-}
    
--StateT {runStateT :: s -> m (a, s)}
--newtype StateT s m a :: * -> (* -> *) -> * -> *
    
--sequenceA :: f (m a) -> m (f a)
sequenceAS :: [StateT s m ma] -> StateT s m [ma]
--sequenceAS [] = StateT (\s->Const [] )
sequenceAS (x:xs) = StateT $ execStateT x _ >>= runStateT (sequenceAS xs)

----Ei t‰st‰ tule yht‰‰n mit‰‰n.









{-You should be able to build f from the following pieces.

    Identity from base
    Const c from base
    Min from base
    ReaderT r m from mtl
    WriterT w m from mtl
    StateT s m from mtl
    Backwards from transformers
-}
