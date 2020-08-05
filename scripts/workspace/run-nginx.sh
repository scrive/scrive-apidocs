#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-`pwd -P`}
NGINX_CONF_DIR="$KONTRAKCJA_WORKSPACE"/scripts/workspace/nginx-conf

mkdir -p /tmp/nginx

echo "Running nginx"
nginx \
    -g "pid $NGINX_CONF_DIR/nginx.pid;" \
    -p "$NGINX_CONF_DIR" \
    -c "$NGINX_CONF_DIR"/nginx.conf
