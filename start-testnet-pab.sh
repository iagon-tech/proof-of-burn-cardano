#!/bin/bash

set -eux

# bin=$(cabal list-bin exe:plutus-pab-examples)
bin=$(cabal list-bin exe:plutus-burner-pab)

"${bin}" \
    --config testnet/pab-config.yml migrate

exec "${bin}" \
    --config testnet/pab-config.yml webserver \
    --passphrase pab123456789


