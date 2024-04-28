{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
import Prelude hiding (reverse)
import Data.Kind
import Data.Proxy
import Data.Type.Equality

data PeanoNat = Zero | Succ PeanoNat

data SPeanoNat (n :: PeanoNat) where
  SZero :: SPeanoNat 'Zero
  SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

type SizedList :: PeanoNat -> Type -> Type
data SizedList n a where
  Nil :: SizedList 'Zero a
  Cons :: a -> SizedList m a -> SizedList ('Succ m) a

--
-- Add n 'Zero ~ n の証明
--

rightZero :: SPeanoNat n -> Add n 'Zero :~: n
rightZero SZero = Refl
rightZero (SSucc s) =
  -- sの型は、 n ~ 'Succ n' となるような n' について SPeanoNat n'
  case rightZero s of
    Refl {- :: Add n' 'Zero :~: n' -} ->
      Refl {- :: Add ('Succ n') 'Zero :~: 'Succ n' -}

--
-- Add n ('Succ m) ~ 'Succ (Add n m) の証明
--

rightSucc :: SPeanoNat n -> Proxy m
          -> Add n ('Succ m) :~: 'Succ (Add n m)
rightSucc SZero _ = Refl
rightSucc (SSucc s) proxy = case rightSucc s proxy of
                              Refl -> Refl

--
-- リストの反転
--

reverse :: SizedList n a -> SizedList n a
reverse xs = revAppend Nil xs

-- リストの長さをシングルトンとして返すヘルパー関数
sizedLength :: SizedList n a -> SPeanoNat n
sizedLength Nil = SZero
sizedLength (Cons _ xs) = SSucc (sizedLength xs)

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = case rightZero (sizedLength acc) of
  Refl -> {- ここで型レベル等式 Add n 'Zero ~ n が利用可能になる -}
          acc
revAppend acc (Cons x (xs :: SizedList m' a)) =
  case rightSucc (sizedLength acc) (Proxy :: Proxy m') of
    Refl -> {- ここで型レベル等式 Add n ('Succ m') ~ 'Succ (Add n m') が利用可能になる -}
            revAppend (Cons x acc) xs

-- 以下、動作確認用のコード

type Three = 'Succ ('Succ ('Succ 'Zero))

someList :: SizedList Three String
someList = Cons "foo" (Cons "bar" (Cons "baz" Nil))

toList :: SizedList n a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

main :: IO ()
main = do print (toList someList)
          print (toList (reverse someList))
