{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Proxy

newtype T = T String deriving (Eq, Show)

instance KnownSymbol s => IsLabel s T where
  fromLabel = T (symbolVal (Proxy :: Proxy s))

main = print (#foo :: T)
