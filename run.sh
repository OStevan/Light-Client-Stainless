#!/bin/bash

stainless-scalac  "$@" --watch \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/integration/ModelIntegration.scala \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/types/*.scala \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/light/*.scala \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/blockchain/*.scala  \
  light-client-core/src/main/scala/ch/epfl/ognjanovic/stevan/tendermint/verified/fork/*.scala  \
  light-client-core/src/main/scala/utils/*.scala
