{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
import GHC.TypeNats
import Numeric.Natural
import Data.Proxy

main = print (natVal (Proxy :: Proxy (16 * 16)) :: Natural)
