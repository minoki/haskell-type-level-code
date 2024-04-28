{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
import GHC.TypeNats
import Data.Proxy

-- 2以上7以下の型レベル自然数を受け取る関数
foo :: (KnownNat a, 2 <= a, a <= 7) => Proxy a -> IO ()
foo proxy = print $ natVal proxy

main = do
  -- foo (Proxy :: Proxy 1) -- コンパイルが通らない
  -- foo (Proxy :: Proxy 10) -- コンパイルが通らない
  foo (Proxy :: Proxy 2) -- コンパイルが通る
