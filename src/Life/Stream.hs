-- | 惰性无穷流（与 match3 的 @Engine.Stream@ 同一小套接口，无项目依赖）。
--
-- 与列表不同：没有空构造，因此 'headS' / 'findS' / 'splitAtS' 都是全函数。
-- 只强制已消费的前缀，其余保持 thunk。
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

-- | 无穷流：一个元素后接另一条流。永不为空。
data Stream a = a :> Stream a

-- | 余代数：种子 @s@ 产出一个元素与下一枚种子。
unfoldS :: (s -> (a, s)) -> s -> Stream a
unfoldS step = go
  where
    go s = let (a, s') = step s in a :> go s'

-- | @x@、@f x@、@f (f x)@、…
iterateS :: (a -> a) -> a -> Stream a
iterateS f = unfoldS (\x -> (x, f x))

-- | 流头（全函数：流永不为空）。
headS :: Stream a -> a
headS (a :> _) = a

-- | 取前 @n@ 个元素（@n <= 0@ → 空列表）。
takeS :: Int -> Stream a -> [a]
takeS n = fst . splitAtS n

-- | 前 @n@ 个元素与剩余流（@n <= 0@ → 空前缀）。
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS n s | n <= 0 = ([], s)
splitAtS n (a :> rest) =
  let (xs, s') = splitAtS (n - 1) rest
  in (a : xs, s')

-- | 第一个满足谓词的元素（若永不满足则一直搜索）。
findS :: (a -> Bool) -> Stream a -> a
findS ok = go
  where
    go (a :> rest)
      | ok a      = a
      | otherwise = go rest
