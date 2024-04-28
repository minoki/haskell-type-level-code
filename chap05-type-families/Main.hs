{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Kind

data PeanoNat = Zero
              | Succ PeanoNat

--
-- シンプルな関数
--

-- type Plus2 :: PeanoNat -> PeanoNat
type Plus2 (n :: PeanoNat) = 'Succ ('Succ n)

--
-- パターンマッチ
--

-- type Pred :: PeanoNat -> PeanoNat
type family Pred (n :: PeanoNat) :: PeanoNat
type instance Pred 'Zero = 'Zero
type instance Pred ('Succ m) = m

{- 参考：同等の値レベル関数
pred :: PeanoNat -> PeanoNat
pred Zero = Zero
pred (Succ m) = m
-}

--
-- 再帰的な定義
--

-- type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

{- 参考：同等の値レベル関数
add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m
add (Succ n') m = Succ (add n' m)
-}

{- 別の定義（要UndecidableInstances）
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = Add n' ('Succ m)
-}

--
-- 閉じた型族
--

type family IsInt (t :: Type) :: Bool where
  IsInt Int = 'True
  IsInt a = 'False

type family Equal a b where
  Equal a a = 'True
  Equal a b = 'False

type family Arity (t :: Type) :: PeanoNat where
  Arity (a -> b) = 'Succ (Arity b)
  Arity a = 'Zero

--
-- 型演算子
--

type family (+) (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance (+) 'Zero m = m
type instance (+) ('Succ n') m = 'Succ (n' + m)

{- 別の定義
type family (n :: PeanoNat) + (m :: PeanoNat) :: PeanoNat
type instance 'Zero + m = m
type instance 'Succ n' + m = 'Succ (n' + m)
-}
