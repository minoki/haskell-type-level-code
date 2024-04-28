{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
import Prelude hiding (replicate)
import qualified Data.List as List
import Data.Proxy
import GHC.TypeNats

-- 長さを型パラメーター（幽霊型）で持つリスト型
-- 不変条件：長さが n である
newtype SizedList (n :: Nat) a = SizedList [a]
  deriving (Eq, Show)

replicate :: forall n a. KnownNat n => a -> SizedList n a
replicate x = SizedList (List.replicate (fromIntegral (natVal (Proxy :: Proxy n))) x)

foo :: forall n. KnownNat n => Proxy n -> Bool
foo _ = let xs = replicate 'a' :: SizedList (2 * n) Char
        in xs == xs

main = print (foo (Proxy :: Proxy 7))
