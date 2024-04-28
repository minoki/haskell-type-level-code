{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
import Data.Proxy

--
-- 長さ n のリスト型
--

newtype SizedList (n :: PeanoNat) a = SizedList [a]

safeHead :: SizedList ('Succ m) a -> a
safeHead (SizedList xs) = head xs

--
-- 型付き構文木
--

data Expr a
  = Const Int -- 不変条件：a = Int
  | Add (Expr Int) (Expr Int) -- 不変条件：a = Int
  | Equal (Expr Int) (Expr Int) -- 不変条件：a = Bool
  | IfThenElse (Expr Bool) (Expr a) (Expr a)

mkConst :: Int -> Expr Int
mkConst = Const

mkAdd :: Expr Int -> Expr Int -> Expr Int
mkAdd = Add

mkEqual :: Expr Int -> Expr Int -> Expr Bool
mkEqual = Equal

--
-- Proxy の例
--

maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)

--
-- 補助的な定義
--

data PeanoNat = Zero | Succ PeanoNat
