-- | Lazy infinite streams (same small surface as match3's Engine.Stream; no dependency).
--
-- Unlike lists there is no empty constructor, so 'headS' / 'findS' / 'splitAtS' are total.
-- Only the consumed prefix is forced; the rest stays as a thunk.
module Life.Stream
  ( Stream (..)
  , unfoldS
  , iterateS
  , headS
  , takeS
  , splitAtS
  , findS
  ) where

infixr 5 :>

-- | Infinite stream: one element followed by another stream. Never empty.
data Stream a = a :> Stream a

-- | Anamorphism: seed @s@ yields one element and the next seed.
unfoldS :: (s -> (a, s)) -> s -> Stream a
unfoldS step = go
  where
    go s = let (a, s') = step s in a :> go s'

-- | @x@, @f x@, @f (f x)@, …
iterateS :: (a -> a) -> a -> Stream a
iterateS f = unfoldS (\x -> (x, f x))

headS :: Stream a -> a
headS (a :> _) = a

-- | First @n@ elements (@n <= 0@ → empty list).
takeS :: Int -> Stream a -> [a]
takeS n = fst . splitAtS n

-- | First @n@ elements and the remainder (@n <= 0@ → empty prefix).
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS n s | n <= 0 = ([], s)
splitAtS n (a :> rest) =
  let (xs, s') = splitAtS (n - 1) rest
  in (a : xs, s')

-- | First element satisfying the predicate (keeps searching forever if none).
findS :: (a -> Bool) -> Stream a -> a
findS ok = go
  where
    go (a :> rest)
      | ok a      = a
      | otherwise = go rest
