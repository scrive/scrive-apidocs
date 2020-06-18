#!/usr/bin/env bash

set -eux

./shake.sh all

supervisorctl -c supervisord.conf start postgres

cabal v2-run kontrakcja-migrate

supervisorctl -c supervisord.conf start all
