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
myMap f = myFoldr (\ h t -> (f h) : t) []
{--   myMap f [] = []
      myMap f (h:t) = f h : myMap f t   -}



myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((++) . f) []
{--   myConcatMap f [] = []
      myConcatMap f (_:t) = concat (map f t)   -}



myConcat :: [[a]] -> [a]
myConcat = myFoldl (\ h t -> h ++ t) []
{--   myConcat [] = []
      myConcat (h:t) = h ++ myConcat t   -}



myReverse :: [a] -> [a]
myReverse = myFoldl rev [] where
    rev t h = (h:t)
{--   lst - список для переворота
      [] - список, где накопливаются перевёрнутый список
      myReverse lst = rev lst [] where
          rev [] a = a
          rev (h:t) a = rev t (h:a)   -}



myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\ h t -> if p h then (h:t) else t) []
{--   p - предикат для сравнения со списком
      myFilter p [] = []
      myFilter p (h:t) | p h = h : myFilter p t | otherwise = myFilter p t   -}



myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr partition' ([], []) where
    partition' h (acc, rej) = if p h then (h:acc, rej) else (acc, h:rej)
{--   p - предикат для сравнения
      myPartition p [] = ([], [])
      myPartition p (h:t) | p h = ((h:accepted), rejected) | otherwise = (accepted, (h:rejected)) where
          (accepted, rejected) = myPartition p t   -}