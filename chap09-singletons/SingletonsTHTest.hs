{- cabal:
build-depends: base, singletons-th, singletons-base
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Singletons
import Data.Singletons.TH (singletons)
import Prelude.Singletons

$(singletons [d|
  data PeanoNat = Zero | Succ PeanoNat

  add :: PeanoNat -> PeanoNat -> PeanoNat
  add Zero m = m
  add (Succ n) m = Succ (add n m)
  |])
