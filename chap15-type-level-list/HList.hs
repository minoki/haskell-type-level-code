{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Kind

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

type RevAppend :: [k] -> [k] -> [k]
type family RevAppend xs ys
type instance RevAppend '[] ys = ys
type instance RevAppend (x ': xs) ys = RevAppend xs (x ': ys) -- 要 UndecidableInstances

type Reverse :: [k] -> [k]
type Reverse xs = RevAppend xs '[]

--
-- ヘテロジニアスリスト
--

type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

--
-- 連結操作
--

append :: HList xs -> HList ys -> HList (xs ++ ys)
append HNil ys = ys
append (HCons x xs) ys = HCons x (append xs ys)

--
-- リストの反転
--

hRevAppend :: HList xs -> HList ys
           -> HList (RevAppend xs ys)
hRevAppend HNil ys = ys
hRevAppend (HCons x xs) ys = hRevAppend xs (HCons x ys)

hReverse :: HList xs -> HList (Reverse xs)
hReverse xs = hRevAppend xs HNil
