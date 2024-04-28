{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Kind
import Data.Proxy
import Data.Type.Equality

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

type ProxyList :: [k] -> Type
data ProxyList xs where
  PNil :: ProxyList '[]
  PCons :: Proxy x -> ProxyList xs -> ProxyList (x ': xs)

type KnownList :: forall {k}. [k] -> Constraint
class KnownList xs where
  proxyList :: ProxyList xs
instance KnownList '[] where
  proxyList = PNil
instance KnownList xs => KnownList (x ': xs) where
  proxyList = PCons Proxy proxyList

--
-- リスト連結の結合法則の証明
--

appendIsAssociative
  :: ProxyList xs -> proxy2 ys -> proxy3 zs
  -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
appendIsAssociative PNil _ _ = Refl
appendIsAssociative (PCons _ xss) ys zs
  = case appendIsAssociative xss ys zs of Refl -> Refl

--
-- 2回 Reverse すると戻ることの証明
--

reverseIsInvolution :: ProxyList xs
                    -> Reverse (Reverse xs) :~: xs
reverseIsInvolution PNil = Refl
reverseIsInvolution (PCons x xss) =
  case revAppendIsReverseAppend xss (PCons x PNil) of
    Refl -> case reverseIsAntihom (pReverse xss) (PCons x PNil) of
      Refl -> case reverseIsInvolution xss of
        Refl -> Refl

--
-- 補助定理、補助関数たち
--

pAppend :: ProxyList xs -> ProxyList ys -> ProxyList (xs ++ ys)
pAppend PNil ys = ys
pAppend (PCons x xs) ys = PCons x (pAppend xs ys)

pRevAppend :: ProxyList xs -> ProxyList ys -> ProxyList (RevAppend xs ys)
pRevAppend PNil ys = ys
pRevAppend (PCons x xs) ys = pRevAppend xs (PCons x ys)

pReverse :: ProxyList xs -> ProxyList (Reverse xs)
pReverse xs = pRevAppend xs PNil

revAppendIsReverseAppend :: ProxyList xs -> ProxyList ys
  -> RevAppend xs ys :~: Reverse xs ++ ys
revAppendIsReverseAppend PNil _ = Refl
revAppendIsReverseAppend (PCons x xss) ys =
  case revAppendIsReverseAppend xss (PCons x ys) of
    Refl -> case revAppendIsReverseAppend xss (PCons x PNil) of
      Refl -> case appendIsAssociative (pReverse xss) (PCons x PNil) ys of
        Refl -> Refl

reverseIsAntihom :: ProxyList xs -> ProxyList ys
  -> Reverse (xs ++ ys) :~: Reverse ys ++ Reverse xs
reverseIsAntihom PNil ys = case appendRightIdentity (pReverse ys) of Refl -> Refl
reverseIsAntihom (PCons x xss) ys =
  case revAppendIsReverseAppend (pAppend xss ys) (PCons x PNil) of
    Refl -> case reverseIsAntihom xss ys of
      Refl -> case appendIsAssociative (pReverse ys) (pReverse xss) (PCons x PNil) of
        Refl -> case revAppendIsReverseAppend xss (PCons x PNil) of
          Refl -> Refl

appendRightIdentity :: ProxyList xs -> xs :~: xs ++ '[]
appendRightIdentity PNil = Refl
appendRightIdentity (PCons x xs) = case appendRightIdentity xs of Refl -> Refl
