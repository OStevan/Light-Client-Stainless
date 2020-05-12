#!/bin/bash

stainless-scalac  "$@" --batched --watch --solvers=smt-z3 \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/integration/ModelIntegration.scala \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/types/*.scala \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/blockchain/*.scala  \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/light/*.scala \
  verified/src/main/scala/utils/*.scala
