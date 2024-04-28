{- cabal:
build-depends: base, open-union
-}
{-# LANGUAGE DataKinds #-}
import Data.OpenUnion

-- Int, String, Charのいずれかを格納できる型
type MyUnion = Union '[Int, String, Char]

someList :: [MyUnion]
someList = [liftUnion "Hello", liftUnion 'x', liftUnion (42 :: Int)]
