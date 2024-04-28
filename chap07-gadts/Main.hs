{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
import Data.Kind (Type)
import Data.Proxy
import Data.Type.Equality
import Prelude hiding (filter)

data PeanoNat = Zero
              | Succ PeanoNat

--
-- 型付きの構文木
--

data Expr (a :: Type) where
  Const :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a

--
-- パターンマッチの例
--

eval :: Expr a -> a
eval e = case e of
  Const x -> {- ここでは a が Int となっている -}
             x
  Add e1 e2 -> {- ここでは a が Int となっている -}
               eval e1 + eval e2
  Equal e1 e2 -> {- ここでは a が Bool となっている -}
                 eval e1 == eval e2
  IfThenElse cond then_ else_ ->
    {- ここでは a は抽象的な型のまま -}
    if eval cond then eval then_ else eval else_

--
-- 長さがちょうど n のリスト型
--

data SizedList (n :: PeanoNat) a where
   Nil :: SizedList 'Zero a
   Cons :: a -> SizedList n a -> SizedList ('Succ n) a

--
-- 型レベル自然数の等価性を判定する関数
--

-- 要 MultiParamTypeClasses ほか
class SameNat (n :: PeanoNat) (m :: PeanoNat) where
  sameNat :: Proxy n -> Proxy m -> Maybe (n :~: m)

instance SameNat 'Zero 'Zero where
  sameNat _ _ = Just Refl

instance SameNat 'Zero ('Succ m) where
  sameNat _ _ = Nothing

instance SameNat ('Succ n) 'Zero where
  sameNat _ _ = Nothing

instance SameNat n m => SameNat ('Succ n) ('Succ m) where
  sameNat _ _ = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy m) of
    Just Refl -> {- ここでは n と m が同じ型となっている -}
      Just Refl {- この Refl は 'Succ n :~: 'Succ m 型だが、 n と m が同じ型なのでコンパイルが通る -}
    Nothing -> Nothing

--
-- 存在型による filter 関数
--

data SomeSizedList a
  = forall n. SomeSizedList (SizedList n a)

filter :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter f Nil = SomeSizedList Nil
filter f (Cons x xs)
  | f x = case filter f xs of
            SomeSizedList ys -> SomeSizedList (Cons x ys)
  | otherwise = filter f xs

--
-- 型制約を含んだデータ構築子
--

data Showable = forall a. Show a => Showable a

showSomething :: Showable -> String
showSomething (Showable x) = show x

--
-- ある型 a が Show クラスのインスタンスであることの「証拠」を持ったデータ型
--

data ShowWitness a = Show a => ShowWitness

-- a に Show 制約は課されていない
showSomethingWithWitness :: ShowWitness a -> a -> String
showSomethingWithWitness x y = case x of
  ShowWitness -> {- ここでは a は Show のインスタンス -}
                 show y
