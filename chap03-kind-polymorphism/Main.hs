{-# LANGUAGE GHC2021 #-}
import Data.Kind

data MyProxy3 (a :: k) = MyProxy3

type MyProxy4 :: k -> Type
data MyProxy4 a = MyProxy4

type MyProxy5 :: forall {k}. k -> Type
data MyProxy5 a = MyProxy5

--
-- 依存カインドの例
--

data T k (a :: k) = MkT
