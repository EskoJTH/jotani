module V6T3 where
--import Data.Fix
{-
Use

data Fix f = Fix (f (Fix f))

instead of explicit recursion to implement the following types.

    data List a = Nil | Cons a (List a)
    data Tree a = Leaf a | Branch (Tree a) (Tree a)
    data Pair a b = Pair a b
-}

data Fix f = Fix (f (Fix f))
data Id a = Id a

type List a = Fix (ListF a)
data ListF a r  = Nil | Cons a r

testList1 :: List a
testList1 = Fix Nil
testList2 :: List Int
testList2 = Fix $ Cons 2 $ Fix Nil
testList3 :: List Int
testList3 = Fix $ Cons 2 $ Fix $ Cons 3 $ Fix Nil

type Tree a = Fix (TreeF a)
data TreeF a r = Leaf a | Branch r r

testTree1 :: Tree Int
testTree1 = Fix (Leaf 1)
testTree2 :: Tree Int
testTree2 = Fix $ Branch (Fix (Leaf 1)) (Fix (Leaf 2))
testTree3 :: Tree Int
testTree3 = Fix $ Branch (Fix (Leaf 1)) (Fix $ Branch (Fix (Leaf 2)) (Fix (Leaf 3))) 

--data Pair a b = Pair a b
type Pair a b = Fix (PairF a b)
data PairF a b r = PairF a b

testPair1 :: Pair Int Bool
testPair1 = Fix (PairF 1 True)

--Eip‰ t‰m‰k‰‰n vaikea ollut kun keksi mitenk‰ tuo eka piti tehd‰. En olisi itse keksinyt ett‰ tuon varsinaisen vekottimen joka sis‰lt‰‰ tuon fixin piti olla tyyppi synonyymi.

--joitain ep‰onnisia yritelmi‰
--data List a = Nil | Cons a (r a)
--data ListFix a = Nil | Cons a deriving Show
--test3 :: List Int
--test3 = Cons 1 $ Fix Nil
--data ListF a = Nil | Cons a (Fix ListF)
--data ListF' r a = Fix Help a
--data ListF a = ListF (Fix ListFix)
--data ListF a = ListF (Fix a)
--data ListF = ListF (Fix ListFix Int)
--data ListF a = ListF (Fix (ListFix a))
--data ListFix recu = Nil | Cons Int (recu Int)
--testList1 = ListF $ Fix Nil
--testist2 :: Fix (ListFix Int)
--testLIst2 = ListF $ Fix Nil
--testList3 = ListF $ Fix (Cons (Fix (Const _ _)) (Nil))
--test2 = Cons 2 Nil
--test3 = Cons 1 $ Cons 3 $ Cons 2 Nil
