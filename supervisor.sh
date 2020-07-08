#!/usr/bin/env bash

set -eux

if [ ! -f supervisor/supervisor.sock ]
then
  supervisord
fi

supervisorctl -c supervisord.conf start postgres

cabal v2-run kontrakcja-migrate

supervisorctl -c supervisord.conf start all
