module V6T2 where

fibonacci' :: [Integer]
fibonacci' = 1 : 1 : recurse 1 1 where
  recurse a b = a+b : recurse b (a+b)

fibonacci'' = 1 : 1 : fib' 1 1
fib' a b = a + b : fib' b (a+b)

fix :: (a -> a) -> a
fix f = f (fix f)

fibonacci :: [Integer]
fibonacci = 1 : 1: fix fib 1 1
fib :: (Integer -> Integer -> [Integer]) -> Integer -> Integer ->[Integer]
fib recu a b = (a + b) : recu b (a+b)


{-fibonacci = [Integer]
fibonacci = 1 : 1 : fix (recurse) where
  recurse a = a+b -}
take' :: Integer -> [a] -> [a]
take' n list= fix take'' n list
take'' recu n [] = []
take'' recu 0 (x:xs) = []
take'' recu n (x:xs) = x:recu (n-1) xs

destutter :: Eq a=> [a]->[a]
destutter (x:xs) = x : fix stutter x xs
stutter recu x [] = []
stutter recu a (x:xs) = if (a==x) then recu a (xs) else x:(recu x xs)

--Olipas n‰m‰ ihan helppoja teht‰vi‰ kun katsoi v‰h‰n luentovideota ja tajus ett‰ mitenk‰ p‰in t‰m‰n fixin on t‰ss‰ tarkoitus olla. Ei aluksi meinannut mill‰‰n onnistua kun en ihan tiennyt mihin v‰liin tuo fix piti laittaa.
