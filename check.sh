#!/bin/bash
set -e
ghc=${1:-ghc}
ghcopt="-XHaskell2010 -Wall -Wcompat"
$ghc --interactive $ghcopt chap02-phantom-types-and-proxy/Main.hs < /dev/null
$ghc --interactive $ghcopt chap03-kind-polymorphism/Main.hs < /dev/null
$ghc --interactive $ghcopt chap05-type-families/Main.hs < /dev/null
$ghc --interactive $ghcopt chap06-type-classes/Main.hs < /dev/null
$ghc --interactive $ghcopt chap07-gadts/Main.hs < /dev/null
$ghc --interactive $ghcopt chap08-examples/Main.hs < /dev/null
$ghc --interactive $ghcopt chap09-singletons/Main.hs < /dev/null
[ "$ghc" = "ghc-9.10" ] || cabal repl -w "$ghc" --ghc-options="$ghcopt" chap09-singletons/SingletonsTHTest.hs < /dev/null
$ghc --interactive $ghcopt chap10-theorem-proving/Main.hs < /dev/null
! $ghc -fno-code $ghcopt chap12-ghc-typenats/LargePeanoNat.hs # -> エラー
$ghc -fno-code $ghcopt chap12-ghc-typenats/LargeNat.hs
$ghc -fno-code $ghcopt chap12-ghc-typenats/LimitRange.hs
! $ghc -fno-code $ghcopt chap12-ghc-typenats/NoEquality.hs
! $ghc -fno-code $ghcopt chap12-ghc-typenats/NoKnownNat.hs
[ "$ghc" = "ghc-9.10" ] || cabal run -w "$ghc" --ghc-options="$ghcopt" chap12-ghc-typenats/PluginExample.hs
[ "$ghc" = "ghc-9.10" ] || ! cabal run -w "$ghc" --ghc-options="$ghcopt" chap12-ghc-typenats/PluginExample2.hs
[ "$ghc" = "ghc-9.10" ] || cabal run -w "$ghc" --ghc-options="$ghcopt" chap12-ghc-typenats/SingletonsBaseExample.hs
[ "$ghc" = "ghc-9.10" ] || cabal run -w "$ghc" --ghc-options="$ghcopt" chap12-ghc-typenats/TypeNaturalExample.hs
[ "$ghc" = "ghc-9.10" ] || cabal run -w "$ghc" --ghc-options="$ghcopt" chap12-ghc-typenats/ConstraintsExample.hs
$ghc -fno-code $ghcopt chap13-symbol/OverloadedLabels.hs
$ghc --interactive $ghcopt chap14-ghc-generics/ConstructorName.hs < /dev/null
$ghc --interactive $ghcopt chap15-type-level-list/reverse.hs < /dev/null
$ghc --interactive $ghcopt chap15-type-level-list/HList.hs < /dev/null
$ghc --interactive $ghcopt chap15-type-level-list/List.hs < /dev/null
$ghc --interactive $ghcopt chap16-typeable/PrintType.hs < /dev/null
$ghc --interactive $ghcopt chap16-typeable/ToString.hs < /dev/null
$ghc --interactive $ghcopt chap16-typeable/ToStringAlt.hs < /dev/null
cabal repl -w "$ghc" --ghc-options="$ghcopt" chap16-typeable/OpenUnion.hs < /dev/null
$ghc --interactive $ghcopt chap17-theorem-proving-2/List.hs < /dev/null
if [ "$ghc" = "ghc-9.10" ]
then
  $ghc --interactive $ghcopt chap18-dependent-haskell/Id.hs < /dev/null  # GHC 9.10+
fi
