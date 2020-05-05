#!/bin/bash

echo -e "-----BEGIN CERTIFICATE-----\n$(cat | fold -w 64)\n-----END CERTIFICATE-----" | openssl x509 -noout -pubkey
