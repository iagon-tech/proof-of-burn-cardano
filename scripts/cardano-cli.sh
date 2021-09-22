#!/bin/sh

set -eu

if ! docker ps -q --no-trunc | grep -q "$(docker-compose ps -q cardano-node)" ; then
	(>&2 echo "Starting docker containers.")
	docker-compose up --build -d 2>&1
	(>&2 echo "Waiting a couple seconds...")
	sleep 5
fi

docker-compose run \
	-v "$(pwd)":/pwd:ro \
	-v "$(pwd)"/out:/pwd/out \
	-w /pwd \
	-u "$(id -u "${USER}")":"$(id -g "${USER}")" \
	-e WALLET_URL=http://cardano-wallet:8090 \
	-e BLOCKFROST_API_TOKEN=${BLOCKFROST_API_TOKEN} \
	-e "${BLOCKFROST_TESTNET_URL:=https://cardano-testnet.blockfrost.io}" \
	-e "${BLOCKFROST_MAINNET_URL:=https://cardano-mainnet.blockfrost.io}" \
	cardano-cli \
	sh -c "$@"
