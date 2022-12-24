module Part1.Tasks where

import Util(notImplementedYet)

factorial :: Double -> Double
factorial fact = if fact <= 1 then 1 else fact * factorial (fact - 1)

-- синус числа (формула Тейлора)
    -- sin(x) = (x^1)/1! - (x^3)/3! + (x^5)/5! - (x^7)/7! + (x^9)/9! - (x^11)/11! + ...
mySin :: Double -> Double
mySin x = taylorSin x 0.0

    -- y - порядок разложения
taylorSin :: Double -> Double -> Double
taylorSin x y = if y == 7
    then
        ((-1) ** y * x ** (2 * y + 1)) / factorial (2 * y + 1)
    else
        ((-1) ** y * x ** (2 * y + 1)) / factorial (2 * y + 1) + taylorSin x (y + 1)



-- косинус числа (формула Тейлора)
    -- cos(x) = (x^0)/0! - (x^2)/2! + (x^4)/4! - (x^6)/6! + (x^8)/8! - (x^10)/10! + ...
myCos :: Double -> Double
myCos x = taylorCos x 0.0

    -- y - порядок разложения
taylorCos :: Double -> Double -> Double
taylorCos x y = if y == 7
    then
        ((-1) ** y * x ** (2 * y)) / factorial (2 * y)
    else
        ((-1) ** y * x ** (2 * y)) / factorial (2 * y) + taylorCos x (y + 1)



-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD m n | m == 0 = abs n
    | otherwise = if n == 0 then abs m else myGCD n $ m `mod` n



-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | day < 1 || day > 31 = False
    | month < 1 || month > 12 = False
    | year < 1 = False
    | month == 2 = checkMonthFebruary day year
    | checkMonth month == True && day < 31 = True
    | checkMonth month == False && day < 30 = True
    | otherwise = False

checkMonth :: Integer -> Bool
checkMonth month | month < 8 && month `mod` 2 /= 0 = True
    | month >= 8 && month `mod` 2 == 0 = True
    | otherwise = False

checkMonthFebruary :: Integer -> Integer -> Bool
checkMonthFebruary day year | year `mod` 400 == 0 && day <= 29 = True
    | year `mod` 100 == 0 && day <= 28 = True
    | year `mod` 100 == 0 && day > 28 = False
    | year `mod` 4 == 0 && day <= 29 = True
    | otherwise = False



-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow m n | n == 0 = 1
    | n == 1 = m
    | otherwise = m * myPow m (n - 1)



-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = isPrimeFor n 2

isPrimeFor :: Integer -> Integer -> Bool
isPrimeFor m i | m == i = True
    | m `mod` i == 0 = False
    | otherwise = isPrimeFor m $ i + 1



type Point2D = (Double, Double)
-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
    let
        m = xGauss points 0 1
        n = yGauss points 1 0
    in
        abs (m - n) / 2

xGauss :: [Point2D] -> Int -> Int -> Double
xGauss points xcoord ycoord | xcoord == length points = 0
    | ycoord == length points = xGauss points xcoord 0
    | otherwise = multiplicationPair points xcoord ycoord + xGauss points (xcoord + 1) (ycoord + 1)

yGauss :: [Point2D] -> Int -> Int -> Double
yGauss points xcoord ycoord | xcoord == length points = yGauss points 0 ycoord
    | ycoord == length points = 0
    | otherwise = multiplicationPair points xcoord ycoord + yGauss points (xcoord + 1) (ycoord + 1)

multiplicationPair :: [Point2D] -> Int -> Int -> Double
multiplicationPair points xcoord ycoord = fst(points !! xcoord) * snd(points !! ycoord)



-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник

    -- a^2+b^2>c^2 - тупоугольный (0)
    -- a^2+b^2<c^2 - остроугольный (1)
    -- a^2+b^2=c^2 - прямоугольный (2)

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | a >= b + c || b >= a + c || c >= a + b = -1
    | a >= b && a >= c = checkTriangle a b c
    | b >= a && b >= c = checkTriangle b a c
    | c >= a && c >= b = checkTriangle c a b
    | otherwise = -1

checkTriangle :: Double -> Double -> Double -> Integer
checkTriangle a b c | b * b + c * c > a * a = 1
    | b * b + c * c < a * a = 0
    | b * b + c * c == a * a = 2
    | otherwise = -1

--triangleKind a b c | a >= b + c || b >= a + c || c >= a + b = -1
--    | a * a + b * b > c * c || b * b + c * c > a * a || a * a + c * c > b * b = 0
--    | a * a + b * b < c * c || b * b + c * c < a * a || a * a + c * c < b * b = 1
--    | a * a + b * b == c * c || b * b + c * c == a * a || a * a + c * c == b * b = 2
--    | otherwise = -1