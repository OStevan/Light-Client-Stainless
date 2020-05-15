#!/bin/bash

stainless-scalac  "$@" --batched --watch --solvers=smt-z3 \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/integration/ModelIntegration.scala \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/types/*.scala \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/blockchain/*.scala  \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/light/*.scala \
  light-client-core/src/main/scala/utils/*.scala
