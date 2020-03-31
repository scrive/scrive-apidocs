#!/usr/bin/env bash

export KONTRAKCJA_ROOT="$( cd "$(dirname "${BASH_SOURCE[0]}")/../.." ; pwd -P )"
export KONTRAKCJA_WORKSPACE="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

echo "KONTRAKCJA_ROOT:" $KONTRAKCJA_ROOT
echo "KONTRAKCJA_WORKSPACE:" $KONTRAKCJA_WORKSPACE
