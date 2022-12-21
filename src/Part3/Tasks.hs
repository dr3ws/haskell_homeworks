module Part3.Tasks where

import Data.List (nub, maximumBy)
import Data.Function (on)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел
-- [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)



-- Функция ff принимает на вход функцию f и элемент x и возвращает список
-- [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)



-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах
-- (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = error "ERROR!!"
mostFreq [x] = x
mostFreq lst = fst $ maximumBy (compare `on` snd) elemCount where
    elemCount = nub [(elem, count) | elem <- lst', let count = length (filter (==elem) lst')] where
        lst' = concatMap digitCount lst

--digitCount :: Int -> Int
--digitCount t = length [h | h <- t, h `elem` [0..9]]
digitCount :: Integral a => a -> [a]
digitCount 0 = []
digitCount x = x `mod` 10 : digitCount (x `div` 10)



-- Дан список lst. Вернуть список элементов из lst без повторений
-- порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (h:t) = if h `elem` t then uniq t else (h:) $ uniq t



-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
-- НЕ РАБОТАЕТ -----------------------------------------------------------------------------------------
grokBy f [] = []
grokBy f (h:t) = if f h then [ (x, xs) | x <- h, xs <- h ] else grokBy f t

--grokBy f l = notImplementedYet
