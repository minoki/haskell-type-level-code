{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

--
-- 型レベルリストの連結
--

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

--
-- 型レベルリストの反転
--

type RevAppend :: [k] -> [k] -> [k]
type family RevAppend xs ys
type instance RevAppend '[] ys = ys
type instance RevAppend (x ': xs) ys = RevAppend xs (x ': ys) -- 要 UndecidableInstances

type Reverse :: [k] -> [k]
type Reverse xs = RevAppend xs '[]

type SlowReverse :: [k] -> [k]
type family SlowReverse xs
type instance SlowReverse '[] = '[]
type instance SlowReverse (x ': xs) = SlowReverse xs ++ '[x]

type X8 = '[0,1,2,3,4,5,6,7]
type X32 = X8 ++ X8 ++ X8 ++ X8
type X128 = X32 ++ X32 ++ X32 ++ X32
