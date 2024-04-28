{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Kind
import Data.Proxy

data PeanoNat = Zero | Succ PeanoNat

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)

type Mul :: PeanoNat -> PeanoNat -> PeanoNat
type family Mul n m
type instance Mul Zero m = Zero
type instance Mul (Succ n) m = Add m (Mul n m)

type One = 'Succ 'Zero
type Two = 'Succ One
type Four = Mul Two Two
type N16 = Mul Four Four -- 4 * 4 = 16
type N256 = Mul N16 N16 -- 16 * 16 = 256

type PeanoNatToInteger :: PeanoNat -> Constraint
class PeanoNatToInteger n where
  natToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  natToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  natToInteger _ = 1 + natToInteger (Proxy :: Proxy n)

main = print (natToInteger (Proxy :: Proxy N256))
