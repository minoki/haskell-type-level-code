# 「Haskellでの型レベルプログラミング」サンプルコード

## 第1章 型とカインド

## 第2章 幽霊型とProxy

```
$ ghci chap02-phantom-types-and-proxy/Main.hs
```

## 第3章 発展：カインド多相

```
$ ghci chap03-kind-polymorphism/Main.hs
```

## 第4章 データ型の昇格（DataKinds拡張）

## 第5章 型レベル関数と型族、型演算子

```
$ ghci chap05-type-families/Main.hs
```

## 第6章 型レベル計算の結果を実行時に利用する：型クラス

```
$ ghci chap06-type-classes/Main.hs
```

## 第7章 一般化された代数的データ型（GADT）

```
$ ghci chap07-gadts/Main.hs
```

## 第8章 復習・小まとめ

```
$ ghci chap08-examples/Main.hs
```

## 第9章 シングルトン型と依存型の模倣

```
$ ghci chap09-singletons/Main.hs
$ cabal repl chap09-singletons/SingletonsTHTest.hs
```

## 第10章 定理証明

```
$ ghci chap10-theorem-proving/Main.hs
```

## 第11章 応用：Constraintカインド

## 第12章 応用：GHCの型レベル自然数（Natカインド）

```
$ runghc chap12-ghc-typenats/LargePeanoNat.hs # -> エラー
$ runghc chap12-ghc-typenats/LargeNat.hs
$ runghc chap12-ghc-typenats/LimitRange.hs
$ runghc chap12-ghc-typenats/NoEquality.hs
$ runghc chap12-ghc-typenats/NoKnownNat.hs
$ cabal run chap12-ghc-typenats/PluginExample.hs
$ cabal run chap12-ghc-typenats/PluginExample2.hs
$ cabal run chap12-ghc-typenats/SingletonsBaseExample.hs
$ cabal run chap12-ghc-typenats/TypeNaturalExample.hs
$ cabal run chap12-ghc-typenats/ConstraintsExample.hs
```

## 第13章 応用：GHCの型レベル文字列（Symbolカインド）

```
$ runghc chap13-symbol/OverloadedLabels.hs
```

## 第14章 応用：GHC.Generics

```
$ ghci chap14-ghc-generics/ConstructorName.hs
```

## 第15章 応用：型レベルリスト

```
$ ghci chap15-type-level-list/reverse.hs
$ ghci chap15-type-level-list/HList.hs
$ ghci chap15-type-level-list/List.hs
```

## 第16章 応用：実行時型情報

```
$ ghci chap16-typeable/PrintType.hs
$ ghci chap16-typeable/ToString.hs
$ ghci chap16-typeable/ToStringAlt.hs
$ cabal repl chap16-typeable/OpenUnion.hs
```

## 第17章 定理証明の代償

```
$ ghci chap17-theorem-proving-2/List.hs
```

## 第18章 Dependent Haskellへの展望

GHC 9.10以降が必要です。

```
$ ghci chap18-dependent-haskell/Id.hs
```
