#!/bin/bash

stainless-scalac  "$@" --batched --watch --solvers=smt-z3 \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/integration/ModelIntegration.scala \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/types/*.scala \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/blockchain/*.scala  \
  verified/src/main/scala/ch/epfl/ognjanovic/stevan/light/*.scala \
  verified/src/main/scala/utils/*.scala
