{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (compare, even, filter, reverse)
import Data.Proxy
import Data.Kind (Type)

--
-- ペアノ流の自然数
--

data PeanoNat = Zero | Succ PeanoNat

{- DataKinds拡張により得られる型構築子：
type 'Zero :: PeanoNat
type 'Succ :: PeanoNat -> PeanoNat
-}

--
-- 1小さい自然数（ただし0に対しては0）を返す関数
--

pred :: PeanoNat -> PeanoNat
pred Zero = Zero
pred (Succ m) = m

type Pred :: PeanoNat -> PeanoNat
type family Pred n
type instance Pred 'Zero = 'Zero
type instance Pred ('Succ m) = m

--
-- 自然数の加法
--

add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m 
add (Succ n') m = Succ (add n' m)

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

{- 別の定義の仕方（要UndecidableInstances）
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = Add n' ('Succ m)
-}

--
-- 自然数の乗算
--

mul :: PeanoNat -> PeanoNat -> PeanoNat
mul Zero m = Zero
mul (Succ n') m = add m (mul n' m)

type Mul :: PeanoNat -> PeanoNat -> PeanoNat
type family Mul n m
type instance Mul 'Zero m = 'Zero
type instance Mul ('Succ n') m = Add m (Mul n' m) -- 要UndecidableInstances

--
-- 自然数の大小比較
--

compare :: PeanoNat -> PeanoNat -> Ordering
compare Zero Zero = EQ
compare Zero (Succ _) = LT
compare (Succ _) Zero = GT
compare (Succ n) (Succ m) = compare n m

type Compare :: PeanoNat -> PeanoNat -> Ordering
type family Compare n m
type instance Compare 'Zero 'Zero = 'EQ
type instance Compare 'Zero ('Succ _) = 'LT
type instance Compare ('Succ _) 'Zero = 'GT
type instance Compare ('Succ n) ('Succ m) = Compare n m

--
-- 与えられた自然数が偶数かどうか
--

even :: PeanoNat -> Bool
even Zero = True
even (Succ Zero) = False
even (Succ (Succ n)) = even n

type Even :: PeanoNat -> Bool
type family Even n
type instance Even 'Zero = 'True
type instance Even ('Succ 'Zero) = 'False
type instance Even ('Succ ('Succ n)) = Even n

--
-- 型レベル自然数を値レベルの自然数に変換する
--

class PeanoNatToInteger (n :: PeanoNat) where
  peanoNatToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  peanoNatToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  peanoNatToInteger _ = 1 + peanoNatToInteger (Proxy :: Proxy n)

--
-- 長さの決まったリスト型
--

type SizedList :: PeanoNat -> Type -> Type
data SizedList n a where
  Nil :: SizedList 'Zero a
  Cons :: a -> SizedList m a -> SizedList ('Succ m) a

--
-- リストの連結
--

append :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

--
-- filter 関数
--

data SomeSizedList a where
  SomeSizedList :: SizedList n a -> SomeSizedList a

filter :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter f Nil = SomeSizedList Nil
filter f (Cons x xs)
  | f x = case filter f xs of
            SomeSizedList ys -> SomeSizedList (Cons x xs)
  | otherwise = filter f xs

--
-- リストの反転
--

{- コンパイルが通らない！
reverse :: SizedList n a -> SizedList n a
reverse xs = revAppend Nil xs

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = acc
revAppend acc (Cons x xs) = revAppend (Cons x acc) xs
-}

--
-- リストの i 番目の要素の取得
--

class Index (i :: PeanoNat) where
  index :: Compare i n ~ 'LT => Proxy i -> SizedList n a -> a

instance Index 'Zero where
  -- index _ Nil = undefined -- GADTによる詳細化と型制約の兼ね合いで到達不能判定されるので書かなくて良い
  index _ (Cons x _) = x

instance Index j => Index ('Succ j) where
  -- index _ Nil = undefined -- GADTによる詳細化と型制約の兼ね合いで到達不能判定されるので書かなくて良い
  index _ (Cons _ xs) = index (Proxy :: Proxy j) xs
