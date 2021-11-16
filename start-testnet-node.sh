#!/bin/bash

set -eux

bin=$(cd "${CARDANO_NODE_DIR}" && cabal list-bin exe:cardano-node)

exec "${bin}" run \
    --config testnet/testnet-config.json \
    --topology testnet/testnet-topology.json \
    --database-path testnet/db \
    --socket-path testnet/node.sock \
    --port 3003
