#!/bin/bash

set -eux

# bin=$(cabal list-bin exe:plutus-pab-examples)
bin=$(cabal list-bin exe:plutus-burner-pab)
pass=$1

"${bin}" \
    --config testnet/pab-config.yml migrate

exec "${bin}" \
    --config testnet/pab-config.yml webserver \
    --passphrase "${pass}"


