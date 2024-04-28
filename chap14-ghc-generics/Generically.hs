{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.Generics
import GHC.TypeLits
import Data.Proxy

genericConstructorName :: (Generic a, ConstructorName' (Rep a)) => a -> String
genericConstructorName = constructorName' . from

class ConstructorName' f where
  constructorName' :: f x -> String

instance KnownSymbol n => ConstructorName' (M1 C (MetaCons n f s) f') where
  constructorName' _ = symbolVal (Proxy :: Proxy n)

instance ConstructorName' f => ConstructorName' (M1 D m f) where
  constructorName' (M1 x) = constructorName' x

instance (ConstructorName' f, ConstructorName' g) => ConstructorName' (f :+: g) where
  constructorName' (L1 x) = constructorName' x
  constructorName' (R1 y) = constructorName' y

class ConstructorName a where
  constructorName :: a -> String

instance (Generic a, ConstructorName' (Rep a)) => ConstructorName (Generically a) where
  constructorName (Generically x) = constructorName' (from x)

data T = A String Int | B | C Char
  deriving Generic
  deriving ConstructorName via Generically T
