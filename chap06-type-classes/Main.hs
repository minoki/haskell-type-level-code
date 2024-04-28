{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
import Data.Proxy

data PeanoNat = Zero
              | Succ PeanoNat

newtype SizedList (n :: PeanoNat) a = SizedList [a]

--
-- PeanoNat カインドの型を Integer に変換する
--

class PeanoNatToInteger (n :: PeanoNat) where
  peanoNatToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  peanoNatToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  -- 要 ScopedTypeVariables 拡張
  peanoNatToInteger _ = 1 + peanoNatToInteger (Proxy :: Proxy n)

lengthOfSizedList :: forall n a. PeanoNatToInteger n => SizedList n a -> Integer
lengthOfSizedList _ = peanoNatToInteger (Proxy :: Proxy n)

--
-- 自然数の大小比較
--

-- 要 MultiParamTypeClasses, ScopedTypeVariables
class CompareNat (n :: PeanoNat) (m :: PeanoNat) where
  compareNat :: Proxy n -> Proxy m -> Ordering

instance CompareNat 'Zero 'Zero where
  compareNat _ _ = EQ

instance CompareNat 'Zero ('Succ m) where
  compareNat _ _ = LT

instance CompareNat ('Succ n) 'Zero where
  compareNat _ _ = GT

instance CompareNat n m => CompareNat ('Succ n) ('Succ m) where
  compareNat _ _ = compareNat (Proxy :: Proxy n) (Proxy :: Proxy m)
