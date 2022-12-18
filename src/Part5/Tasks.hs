module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl a b [] = b
myFoldl a b (h:t) = myFoldl a (a b h) t



-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr a b [] = b
myFoldr a b (h:t) = a h (myFoldr a b t)



-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле
myMap :: (a -> b) -> [a] -> [b]
myMap f = notImplementedYet

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = notImplementedYet

myConcat :: [[a]] -> [a]
myConcat = notImplementedYet

myReverse :: [a] -> [a]
myReverse = notImplementedYet

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = notImplementedYet

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = notImplementedYet

