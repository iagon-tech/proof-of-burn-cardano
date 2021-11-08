#!/bin/bash

set -eux

bin=$(cabal list-bin exe:plutus-chain-index)

exec "${bin}" \
	--config testnet/chain-index-config.json \
	start-index
