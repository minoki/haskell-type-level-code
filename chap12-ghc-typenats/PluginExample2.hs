{- cabal:
build-depends: base
             -- , ghc-typelits-natnormalise
             , ghc-typelits-presburger
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
import Data.Proxy
import GHC.TypeNats

foo :: forall (x :: Nat) (y :: Nat). Proxy x -> Proxy y -> Bool
foo _ _ = let a = Proxy :: Proxy (x * (y + 1))
              b = Proxy :: Proxy ((y + 1) * x)
          in a == b -- a と b は同じ型のはず

main = print (foo (Proxy :: Proxy 3) (Proxy :: Proxy 5))
