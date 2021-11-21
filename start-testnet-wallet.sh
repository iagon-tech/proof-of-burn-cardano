#!/bin/bash

set -eux

bin=$(cabal list-bin exe:cardano-wallet)

exec "${bin}" serve \
    --testnet testnet/testnet-byron-genesis.json \
    --node-socket testnet/node.sock \
    --database testnet/wallet-db
