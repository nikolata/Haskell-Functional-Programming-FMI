import Prelude

summ :: [Int] -> Int
summ [] = 0
summ (x:xs) = x + summ xs

data List a = EmptyList | Cons a (List a)
	deriving Show
sampleList :: List Int
sampleList = Cons 1 (Cons 2 (Cons 3 EmptyList))

--exampleEmptyList :: List Int
exampleEmptyList = EmptyList

sumForList :: List Int -> Int
sumForList EmptyList = 0
sumForList (Cons x xs) = x + sumForList xs


remove' :: Int -> List Int -> List Int
remove' _ EmptyList = EmptyList
remove' e (Cons x lst) | x == e = lst | otherwise = Cons x (remove' e lst)


find :: Int -> List Int -> Int

find _ EmptyList = 0
find e (Cons x lst) = findHelper e 0 (Cons x lst)

findHelper :: Int -> Int -> List Int -> Int
findHelper e ind (Cons x lst) | x == e = ind | otherwise = findHelper e (ind + 1) lst



insert :: (Ord a) => a -> [a] -> [a]
insert el [] = [el]
insert el (x:lst) | el < x = el:x:lst | otherwise = x : insert el lst

-- f :: (Ord a, Eq b) => a -> b -> a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs
