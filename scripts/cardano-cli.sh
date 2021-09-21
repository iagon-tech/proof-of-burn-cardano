#!/bin/sh

set -eu

docker-compose run -v $(pwd):/pwd -w /pwd -u $(id -u ${USER}):$(id -g ${USER}) -e WALLET_URL=http://cardano-wallet:8090 cardano-cli sh -c "$@"
