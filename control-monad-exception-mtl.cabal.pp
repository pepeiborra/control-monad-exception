name: control-monad-exception-mtl
version: 0.8.0.4
Cabal-Version:  >= 1.6
build-type: Simple
license: PublicDomain
author: Pepe Iborra
maintainer: pepeiborra@gmail.com
homepage: http://pepeiborra.github.com/control-monad-exception
synopsis: MTL instances for the EMT exceptions monad transformer
category: Control, Monads, Failure
stability: experimental
tested-with: GHC == 6.12.1
bug-reports: http://github.com/pepeiborra/control-monad-exception/issues
description:
  MTL instances for the EMT exceptions monad transformer
  .
  /This package is only applicable for the now-deprecated mtl v1. Since mtl v2 is compatible with the transformers package, users of mtl2 should simply use the control-monad-exception package.

Library
  buildable: True
  build-depends: base > 4 && < 5
               , control-monad-exception >= 0.8.0 && <= 0.9.0
               , mtl


  extensions:  ScopedTypeVariables,
               PackageImports,
               MultiParamTypeClasses,
               FlexibleContexts,
               FlexibleInstances,
               UndecidableInstances

  exposed-modules:
     Control.Monad.Exception.MTL

  hs-source-dirs: extensions

  ghc-options: -Wall -fno-warn-name-shadowing -fno-warn-orphans



source-repository head
  type:     git
  location: git://github.com/pepeiborra/control-monad-exception.git
