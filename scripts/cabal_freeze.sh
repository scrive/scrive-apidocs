#!/bin/sh -e

cabal freeze

sed -i '
  /any.Cabal ==/d
  /any.array ==/d
  /any.base ==/d
  /any.base-noprelude/d
  /any.binary ==/d
  /any.bytestring ==/d
  /any.containers ==/d
  /any.directory ==/d
  /any.ghc-boot-th ==/d
  /any.parsec ==/d
  /any.process ==/d
  /any.template-haskell ==/d
  /any.text ==/d
  /any.time ==/d
  ' cabal.project.freeze
