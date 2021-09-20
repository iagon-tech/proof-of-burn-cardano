#!/bin/sh

: "${WALLET_URL:=http://localhost:8090}"


create_wallet() {
	curl ${WALLET_URL}/v2/wallets -X POST -H "Content-Type: application/json" --data @<(cat <<EOF
{
	"name": "$1",
	"mnemonic_sentence": $(printf "%s" "$2" | jq -R -s  'split(" ")')
	,
	"passphrase": "$3",
	"address_pool_gap": 20
  }
EOF
)

}

wallets() {
	curl -X GET ${WALLET_URL}/v2/wallets
}

first_wallet_id() {
	wallets | jq -r '.[0].id'
}

addresses() {
	curl -X GET ${WALLET_URL}/v2/wallets/$1/addresses
}

unused_payment_adresses() {
	addresses "$1" | jq -r '[.[] | select( .state | contains("unused"))]'
}

first_unused_payment_address() {
	unused_payment_adresses "$1" | jq -r '.[0].id'
}

wallet() {
	curl -X GET ${WALLET_URL}/v2/wallets/$1
}

available_funds() {
	wallet "$1" | jq '.balance.available.quantity'
}

create_keys() {
	out_dir=$1
	shift
	echo "$@" \
		| cardano-address key from-recovery-phrase Shelley > "$out_dir"/root.prv
	cat "$out_dir"/root.prv \
		| cardano-address key child 1852H/1815H/0H \
		| tee "$out_dir"/acct.prv \
		| cardano-address key public --with-chain-code > "$out_dir"/acct.pub
	cat "$out_dir"/root.prv \
		| cardano-address key child 1852H/1815H/0H/0/0 > "$out_dir"/addr.prv
	cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file "$out_dir"/addr.prv --out-file "$out_dir"/key.skey
	cardano-cli key verification-key --signing-key-file "$out_dir"/key.skey --verification-key-file "$out_dir"/key.vkey
	unset out_dir
}

script_address() {
	out_dir=$1
	if [ "$NETWORK" = "testnet" ] ; then
		cardano-cli address build --payment-script-file "$2" --testnet-magic 1097911063 --out-file "$out_dir"/burn.addr
	else
		cardano-cli address build --payment-script-file "$2" --out-file "$out_dir"/burn.addr
	fi
	unset out_dir
}

payment_address() {
	out_dir=$1
	if [ "$NETWORK" = "testnet" ] ; then
		cardano-cli address build --payment-verification-key-file "$out_dir"/key.vkey --out-file "$out_dir"/payment.addr --testnet-magic 1097911063
	else
		cardano-cli address build --payment-verification-key-file "$out_dir"/key.vkey --out-file "$out_dir"/payment.addr
	fi
	unset out_dir
}

