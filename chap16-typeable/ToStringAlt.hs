{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
import Data.Typeable

toString2 :: (Show a, Typeable a) => a -> String
toString2 x = case cast x :: Maybe String of
                Just s -> s
                Nothing -> show x

toString3 :: forall a. (Show a, Typeable a) => a -> String
toString3 x = case eqT :: Maybe (a :~: String) of
                Just Refl -> x
                Nothing -> show x
