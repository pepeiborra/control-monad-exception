name: control-monad-exception-monadstf
version: 0.8.0.3
Cabal-Version:  >= 1.6
build-type: Simple
license: PublicDomain
author: Pepe Iborra
maintainer: pepeiborra@gmail.com
homepage: http://pepeiborra.github.com/control-monad-exception
description: Monads-tf instances for the EMT exceptions monad transformer
synopsis: Monads-tf instances for the EMT exceptions monad transformer
category: Control, Monads, Failure
stability: experimental
tested-with: GHC == 6.12.1
bug-reports: http://github.com/pepeiborra/control-monad-exception/issues

Library
  buildable: True 
  build-depends: base > 4 && < 5
               , control-monad-exception >= 0.8.0
               , transformers >= 0.1.0
               , monads-tf    >= 0.0.0.1

  extensions:  ScopedTypeVariables, 
               PackageImports,
               MultiParamTypeClasses,
               TypeFamilies,
               FlexibleContexts,
               FlexibleInstances,
               UndecidableInstances

  exposed-modules:
     Control.Monad.Exception.MonadsTF

  hs-source-dirs: src-monadstf

  ghc-options: -Wall -fno-warn-name-shadowing -fno-warn-orphans



source-repository head
  type:     git
  location: git://github.com/pepeiborra/control-monad-exception.git
