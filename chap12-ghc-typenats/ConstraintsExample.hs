{- cabal:
build-depends: base, constraints
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
import Prelude hiding (replicate)
import qualified Data.List as List
import Data.Proxy
import GHC.TypeNats
import Data.Constraint (Dict(Dict), (:-)(Sub))
import Data.Constraint.Nat (timesNat)

-- 長さを型パラメーター（幽霊型）で持つリスト型
-- 不変条件：長さが n である
newtype SizedList (n :: Nat) a = SizedList [a]
  deriving (Eq, Show)

replicate :: forall n a. KnownNat n => a -> SizedList n a
replicate x = SizedList (List.replicate (fromIntegral (natVal (Proxy :: Proxy n))) x)

foo :: forall n. KnownNat n => Proxy n -> Bool
foo _ = case timesNat @2 @n of
  Sub Dict ->
    -- ここで KnownNat (2 * n) が使えるようになる
    let xs = replicate 'a' :: SizedList (2 * n) Char
    in xs == xs

main = print (foo (Proxy :: Proxy 7))
