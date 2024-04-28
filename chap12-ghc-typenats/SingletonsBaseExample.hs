{- cabal:
build-depends: base, singletons-base
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternSynonyms #-}
import Prelude hiding (replicate)
import qualified Data.List as List
import Data.Proxy
import GHC.TypeNats
import Prelude.Singletons ((%*))
#if MIN_VERSION_singletons_base(3, 2, 0)
import GHC.TypeLits.Singletons (SNat, pattern SNat)
#else
import GHC.TypeLits.Singletons (SNat(SNat))
#endif

-- 長さを型パラメーター（幽霊型）で持つリスト型
-- 不変条件：長さが n である
newtype SizedList (n :: Nat) a = SizedList [a]
  deriving (Eq, Show)

replicate :: forall n a. KnownNat n => a -> SizedList n a
replicate x = SizedList (List.replicate (fromIntegral (natVal (Proxy :: Proxy n))) x)

foo :: forall n. KnownNat n => Proxy n -> Bool
foo _ = case SNat @2 %* SNat @n of
          SNat -> -- ここで KnownNat (2 * n) が使えるようになる
            let xs = replicate 'a' :: SizedList (2 * n) Char -- ここで KnownNat (2 * n) が必要になる
            in xs == xs

main = print (foo (Proxy :: Proxy 7))
