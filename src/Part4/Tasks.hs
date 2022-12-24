module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl reversed REmpty where
    reversed h t = (h :< t)



-- Реализуйте все представленные ниже классы (см. тесты)

--  showsPrec :: Int -> a -> ShowS
--  show :: a -> String
instance (Show a) => Show (ReverseList a) where
    showsPrec = notImplementedYet
    show lst = show (rlistToList lst)
{--   show REmpty = "[]"
      show lst = "[" ++ show' lst ++ "]" where
          show' l = case l of
              REmpty -> ""
              (REmpty :< n) -> show n
              (m :< n) -> show' m ++ "," ++ show n   -}


--  (==) :: a -> a -> Bool
--  (/=) :: a -> a -> Bool
instance (Eq a) => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) m n = (rlistToList m) == (rlistToList n)
{--   (==) (m1 :< n1) (m2 :< n2) = m1 == m2 && n1 == n2   -}
    (/=) m n = not (m == n)


--  (<>) :: a -> a -> a
instance Semigroup (ReverseList a) where
    (<>) a REmpty = a
    (<>) REmpty a = a
    (<>) m n = listToRlist ((rlistToList m) ++ (rlistToList n))
{--   (<>) a (m :< n) = (mappend a m) :< n
      (<>) a (m :< n) = (a <> m) :< n   -}


--  mempty :: a
--  mappend :: a -> a -> a
instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend = (<>)


--  fmap :: (a -> b) -> f a -> f b
instance Functor ReverseList where
    fmap f REmpty = REmpty
--  оператор доллар применяет функцию к аргументу, а доллар в угловых скобочках применяет функцию к множеству аргументов
    fmap f a = listToRlist (f <$> (rlistToList a))
{--   fmap f (m :< n) = (fmap f m) :< (f n)   -}


--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b
--  f<*>x = f >>= \f' -> x >>= \x' -> return (f' x')
instance Applicative ReverseList where
    pure = return
    (<*>) _ REmpty = REmpty
    (<*>) REmpty _ = REmpty
{--   (<*>) m n = m >>= \f' -> n >>= \x' -> return $ f' x'   -}
    (<*>) m n = do
          f <- m
          x <- n
          return $ f x


--  (>>=) :: m a -> (a -> m b) -> m b
--  (>>) :: m a -> m b -> m b
--  return :: a -> m a
--  fail :: String -> m a
instance Monad ReverseList where
    (>>=) x f = flatten $ fmap f x --bind
    (>>) m n = m >>= \x' -> n
    return a = REmpty :< a

-- «расплющить» монаду от монады
-- flatten :: m (m a) -> m a
flatten :: ReverseList (ReverseList a) -> ReverseList a
flatten REmpty = REmpty
flatten (REmpty :< n) = n
flatten (m :< n) = flatten m <> n