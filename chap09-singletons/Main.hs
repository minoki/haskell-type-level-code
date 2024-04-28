{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
import Prelude hiding (compare)
import Data.Proxy

data PeanoNat = Zero | Succ PeanoNat

--
-- シングルトン型
--

data SPeanoNat (n :: PeanoNat) where
  SZero :: SPeanoNat 'Zero
  SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)

--
-- 自然数の和
--

-- 型レベル関数
type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

-- 参考：同等の値レベル関数
add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m
add (Succ n') m = Succ (add n' m)

sAdd :: SPeanoNat n -> SPeanoNat m -> SPeanoNat (Add n m)
sAdd SZero m = m
sAdd (SSucc n') m = SSucc (sAdd n' m)

--
-- Bool と Ordering に対応するシングルトン型
--

data SBool (x :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

data SOrdering (x :: Ordering) where
  SLT :: SOrdering 'LT
  SEQ :: SOrdering 'EQ
  SGT :: SOrdering 'GT

--
-- 自然数の比較関数
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

sCompare :: SPeanoNat n -> SPeanoNat m -> SOrdering (Compare n m)
sCompare SZero SZero = SEQ
sCompare SZero (SSucc _) = SLT
sCompare (SSucc _) SZero = SGT
sCompare (SSucc n) (SSucc m) = sCompare n m

--
-- 型に対して対応するシングルトンを返す型クラス
--

class PeanoNatI (n :: PeanoNat) where
  singPeanoNat :: SPeanoNat n

instance PeanoNatI 'Zero where
  singPeanoNat = SZero

instance PeanoNatI n => PeanoNatI ('Succ n) where
  singPeanoNat = SSucc singPeanoNat

class BoolI (x :: Bool) where
  singBool :: SBool x

instance BoolI 'True where
  singBool = STrue
instance BoolI 'False where
  singBool = SFalse

class OrderingI (x :: Ordering) where
  singOrdering :: SOrdering x

instance OrderingI 'LT where
  singOrdering = SLT
instance OrderingI 'EQ where
  singOrdering = SEQ
instance OrderingI 'GT where
  singOrdering = SGT

--
-- 型レベル自然数を値レベルの整数に変換する関数
--

sPeanoNatToInteger :: SPeanoNat n -> Integer
sPeanoNatToInteger SZero = 0
sPeanoNatToInteger (SSucc n') = 1 + sPeanoNatToInteger n'

peanoNatToInteger :: forall n. PeanoNatI n
                  => Proxy n -> Integer
peanoNatToInteger _
  = sPeanoNatToInteger (singPeanoNat :: SPeanoNat n)

--
-- シングルトン SPeanoNat n から PeanoNatI n 制約を捻出する関数
--

data PeanoNatInstance (n :: PeanoNat) where
  PeanoNatInstance :: PeanoNatI n => PeanoNatInstance n

peanoNatInstance :: SPeanoNat n -> PeanoNatInstance n
peanoNatInstance SZero = PeanoNatInstance
peanoNatInstance (SSucc n') = case peanoNatInstance n' of
  PeanoNatInstance -> PeanoNatInstance

--
-- 値レベルの自然数をシングルトンに変換する関数
--

data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). SPeanoNat n -> SomePeanoNat

somePeanoNat :: PeanoNat -> SomePeanoNat
somePeanoNat Zero = SomePeanoNat SZero
somePeanoNat (Succ n) = case somePeanoNat n of
  SomePeanoNat n -> SomePeanoNat (SSucc n)

--
-- 実行時の自然数を型レベルに持ち上げて、 PeanoNatI 制約のついた関数を呼び出す処理
--

peanoNatToInteger' :: PeanoNat -> Integer
peanoNatToInteger' x = case somePeanoNat x of
  SomePeanoNat (s :: SPeanoNat n) ->
    -- ここで n は PeanoNat カインドの型変数
    case peanoNatInstance s of
      PeanoNatInstance -> -- ここで PeanoNatI n 制約が導入される
        peanoNatToInteger (Proxy :: Proxy n)
