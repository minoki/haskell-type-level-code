{-# LANGUAGE GADTs #-}
import Type.Reflection as R
import Data.Type.Equality

toString :: (Show a, Typeable a) => a -> String
toString x = case testEquality (R.typeOf x) (R.typeRep :: R.TypeRep String) of
               Just Refl -> x
               Nothing -> show x
