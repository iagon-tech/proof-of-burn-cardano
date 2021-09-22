#!/bin/sh
# shellcheck disable=SC2015

: "${WALLET_URL:=http://localhost:8090}"
: "${TESTNET_MAGIC:=1097911063}"
: "${BLOCKFROST_TESTNET_URL:=https://cardano-testnet.blockfrost.io}"
: "${BLOCKFROST_MAINNET_URL:=https://cardano-mainnet.blockfrost.io}"
: "${BLOCKFROST_API_TOKEN}"



   #######################
   ## Utility functions ##
   #######################


# @FUNCTION: die
# @USAGE: [msg]
# @DESCRIPTION:
# Exits the shell script with status code 2
# and prints the given message in red to STDERR, if any.
die() {
    (>&2 red_message "$1")
    exit 2
}

# @FUNCTION: edo
# @USAGE: <command>
# @DESCRIPTION:
# Executes the given command. Also prints what
# command that is (in blue) if verbosity is enabled.
# Exits with status code 2 if the command failed.
edo()
{
    "$@" || exit 2
}

# @FUNCTION: status_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a green status message.
status_message() {
    printf "\\033[0;32m%s\\033[0m\\n" "$1"
}

# @FUNCTION: warning_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a yellow warning message.
warning_message() {
    printf "\\033[1;33m%s\\033[0m\\n" "$1"
}

# @FUNCTION: red_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a red message.
red_message() {
    printf "\\033[0;31m%s\\033[0m\\n" "$1"
}

# @FUNCTION: to_hex
# @USAGE: <ascii-string>
# @DESCRIPTION:
# Converts an ascii string to uppercase hex string (no leading  '0x').
# @STDOUT: The converted to hex string
to_hex() {
    [ -z "$1" ] && die "Error: no argument given to to_hex"
	xxd -p <<EOF
$1
EOF
}

# @FUNCTION: from_hex
# @USAGE: <hex-string>
# @DESCRIPTION:
# Converts a hex string to an ascii string.
# @STDOUT: The converted to ascii string
from_hex() {
    [ -z "$1" ] && die "Error: no argument given to from_hex"
	xxd -p -r <<EOF
$1
EOF
}

# @FUNCTION: flip_last_bit
# @USAGE: <hex-string>
# @DESCRIPTION:
# Flips the last bit of a hex string.
# @STDOUT: The hex string with last bit flipped.
flip_last_bit() {
	export BC_LINE_LENGTH=0
    [ -z "$1" ] && die "Error: no argument given to flip_last_bit"
	_binary=$(echo "obase=2; ibase=16; $(echo "$1" | tr 'a-f' 'A-F')" | bc)
	_prefix=$(printf "%s" "$_binary" | head -c -1)
	_tail=$(printf "%s" "$_binary" | tail -c 1)
	# shellcheck disable=SC2004
	_tail_flipped=$(( $_tail ^ 1 ))
	echo "obase=16; ibase=2; ${_prefix}${_tail_flipped}" | bc | tr 'A-F' 'a-f'

	unset _binary _prefix _tail _tail_flipped
}

# @FUNCTION: sha3_256
# @USAGE: <string>
# @DESCRIPTION:
# Hash a string with sha3-256 in hex format.
# @STDOUT: The hashed string.
sha3_256() {
    [ -z "$1" ] && die "Error: no argument given to sha3_256"
	printf "%s" "$1" | openssl dgst -r -sha3-256 | awk '{ print $1 }'
}

# @FUNCTION: datum_hash
# @USAGE: <string>
# @DESCRIPTION:
# Get the datum hash of a string in hex format.
# @STDOUT: The hashed string.
datum_hash() {
    [ -z "$1" ] && die "Error: no argument given to datum_hash"
	cardano-cli transaction hash-script-data --script-data-value "\"$1\""
}

# @FUNCTION: to_burn_datum_hash
# @USAGE: <string>
# @DESCRIPTION:
# Converts burn commitment to a full datum hash ready to be submitted
# in a transaction.
# @STDOUT: The hashed string.
to_burn_datum_hash(){
    [ -z "$1" ] && die "Error: no argument given to to_burn_datum_hash"
	datum_hash "$(flip_last_bit "$(sha3_256 "$1")")"
}



   ######################
   ## Wallet functions ##
   ######################


# @FUNCTION: create_wallet
# @USAGE: <wallet-name> <mnemonic-sentence> <passphrase>
# @DESCRIPTION:
# Creates or restores a wallet from a mnemonic sentence.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postWallet
create_wallet() {
	[ "$#" -lt 3 ] && die "error: not enough arguments to create_wallet (expexted 3)"

	cat <<EOF > /tmp/payload
  {
	"name": "$1",
	"mnemonic_sentence": $(printf "%s" "$2" | jq -R -s  'split(" ")')
	,
	"passphrase": "$3",
	"address_pool_gap": 20
  }
EOF

	curl -s -H "Content-Type: application/json" --data @/tmp/payload "${WALLET_URL}/v2/wallets"

	ret=$?
	rm /tmp/payload

	[ "$ret" -ne 0 ] && die "Failed to create wallet"
	unset ret
}

# @FUNCTION: wallets
# @DESCRIPTION:
# Gets all wallets currently registered in the wallet DB.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listWallets
wallets() {
	curl -s -X GET "${WALLET_URL}/v2/wallets"
}

# @FUNCTION: first_wallet_id
# @DESCRIPTION:
# Gets the id if the first wallet registered in the wallet DB.
# STDOUT: wallet id as a string
first_wallet_id() {
	wallets | jq -r '.[0].id'
}

# @FUNCTION: addresses
# @USAGE: <wallet-id>
# @DESCRIPTION:
# Gets all addresses of a wallet.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listAddresses
addresses() {
    [ -z "$1" ] && die "Error: no argument given to addresses"

	curl -s -X GET "${WALLET_URL}/v2/wallets/$1/addresses"
}

# @FUNCTION: unused_payment_adresses
# @USAGE: <wallet-id>
# @DESCRIPTION:
# Gets all unused payment addresses of a wallet.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listAddresses
unused_payment_adresses() {
    [ -z "$1" ] && die "Error: no argument given to unused_payment_adresses"

	addresses "$1" | jq -r '[.[] | select( .state | contains("unused"))]'
}

# @FUNCTION: first_unused_payment_address
# @USAGE: <wallet-id>
# @DESCRIPTION:
# Get the first unused payment address of a wallet.
# STDOUT: bech32 payment address as a string
first_unused_payment_address() {
    [ -z "$1" ] && die "Error: no argument given to first_unused_payment_address"

	unused_payment_adresses "$1" | jq -r '.[0].id'
}

# @FUNCTION: wallet
# @USAGE: <wallet-id>
# @DESCRIPTION:
# Get the wallet.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getWallet
wallet() {
    [ -z "$1" ] && die "Error: no argument given to wallet"

	curl -s -X GET "${WALLET_URL}/v2/wallets/$1"
}

# @FUNCTION: available_funds
# @USAGE: <wallet-id>
# @DESCRIPTION:
# Get the available funds of a wallet.
# STDOUT: total available funds in lovelace
available_funds() {
    [ -z "$1" ] && die "Error: no argument given to available_funds"

	edo wallet "$1" | jq -r '.balance.available.quantity'
}




   ########################
   ## Blockchain queries ##
   ########################


# @FUNCTION: get_utxo
# @USAGE: <address>
# @DESCRIPTION:
# Get UTxO by address.
get_utxo() {
    [ -z "$1" ] && die "Error: no argument given to get_utxo"

	[ -n "${BLOCKFROST_API_TOKEN}" ] || die "BLOCKFROST_API_TOKEN not set"

	if [ "$NETWORK" = "testnet" ] ; then
		curl -s -X GET -H "project_id: ${BLOCKFROST_API_TOKEN}" \
			"${BLOCKFROST_TESTNET_URL}/api/v0/addresses/$1/utxos"
	else
		curl -s -X GET -H "project_id: ${BLOCKFROST_API_TOKEN}" \
			"${BLOCKFROST_MAINNET_URL}/api/v0/addresses/$1/utxos"
	fi
}





   ############################
   ## Creating a transaction ##
   ############################


# TODO: use wallet API
# @FUNCTION: create_keys
# @USAGE: <out-dir>
# @DESCRIPTION:
# Create signing and verification keys to be used for a transaction.
create_keys() {
    [ -z "$1" ] && die "Error: no argument given to create_keys"

	out_dir=$1
	shift
	edo echo "$@" \
		| cardano-address key from-recovery-phrase Shelley > "$out_dir"/root.prv
	edo cat "$out_dir"/root.prv \
		| cardano-address key child 1852H/1815H/0H \
		| tee "$out_dir"/acct.prv \
		| cardano-address key public --with-chain-code > "$out_dir"/acct.pub
	edo cat "$out_dir"/root.prv \
		| cardano-address key child 1852H/1815H/0H/0/0 > "$out_dir"/addr.prv
	edo cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file "$out_dir"/addr.prv --out-file "$out_dir"/key.skey
	edo cardano-cli key verification-key --signing-key-file "$out_dir"/key.skey --verification-key-file "$out_dir"/key.vkey
	unset out_dir
}

# TODO: use wallet API
# @FUNCTION: script_address
# @USAGE: <out-dir> <plutus-script>
# @DESCRIPTION:
# Create script address of the plutus script to be used in a transaction.
script_address() {
	[ "$#" -lt 2 ] && die "error: not enough arguments to script_address (expexted 2)"

	out_dir=$1
	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli address build --payment-script-file "$2" --testnet-magic "${TESTNET_MAGIC}" --out-file "$out_dir"/burn.addr
	else
		edo cardano-cli address build --payment-script-file "$2" --out-file "$out_dir"/burn.addr
	fi
	unset out_dir
}

# TODO: use wallet API
# @FUNCTION: payment_address
# @USAGE: <out-dir>
# @DESCRIPTION:
# Create a payment address to be used in a transaction.
payment_address() {
    [ -z "$1" ] && die "Error: no argument given to payment_address"

	out_dir=$1
	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli address build --payment-verification-key-file "$out_dir"/key.vkey --out-file "$out_dir"/payment.addr --testnet-magic "${TESTNET_MAGIC}"
	else
		edo cardano-cli address build --payment-verification-key-file "$out_dir"/key.vkey --out-file "$out_dir"/payment.addr
	fi
	unset out_dir
}

# TODO: use wallet API
# @FUNCTION: gen_protocol_parameters
# @USAGE: <out-dir>
# @DESCRIPTION:
# Get the protocol parameters from the network.
gen_protocol_parameters() {
    [ -z "$1" ] && die "Error: no argument given to gen_protocol_parameters"

	out_dir=$1
	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli query protocol-parameters --testnet-magic "${TESTNET_MAGIC}" > "$out_dir"/pparams.json
	else
		edo cardano-cli query protocol-parameters > "$out_dir"/pparams.json
	fi
	unset out_dir
}

# TODO: use wallet API
# @FUNCTION: create_script_transaction
# @USAGE: <out-dir> <tx_in> <addr> <amount> <datum> <change_addr>
# @DESCRIPTION:
# Create a transaction for a plutus script.
create_script_transaction() {
	[ "$#" -lt 6 ] && die "error: not enough arguments to create_script_transaction (expexted 6)"

	gen_protocol_parameters "$1"

	out_dir=$1
	tx_in=$2
	burn_addr=$3
	burn_amount=$4
	burn_datum=$5
	change_addr=$6

	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli transaction build \
			--alonzo-era \
			--tx-in "$tx_in" \
			--tx-out "$burn_addr+$burn_amount" \
			--tx-out-datum-hash "$burn_datum" \
			--change-address "$change_addr" \
			--testnet-magic "${TESTNET_MAGIC}" \
			--protocol-params-file "$out_dir"/pparams.json \
			--out-file "$out_dir"/tx.raw
	else
		edo cardano-cli transaction build \
			--alonzo-era \
			--tx-in "$tx_in" \
			--tx-out "$burn_addr+$burn_amount" \
			--tx-out-datum-hash "$burn_datum" \
			--change-address "$change_addr" \
			--protocol-params-file "$out_dir"/pparams.json \
			--out-file "$out_dir"/tx.raw
	fi

	unset out_dir tx_in burn_addr burn_amount burn_datum change_addr
}

# TODO: use wallet API
# @FUNCTION: redeem_script_transaction
# @USAGE: <out-dir> <tx-in> <script-file> <datum-value> <redeemer-value> <tx-in-collateral> <change-address>
# @DESCRIPTION:
# Create a transaction for a plutus script.
redeem_script_transaction() {
	[ "$#" -lt 7 ] && die "error: not enough arguments to redeem_script_transaction (expexted 7)"

	gen_protocol_parameters "$1"

	out_dir=$1
	tx_in=$2
	script_file=$3
	datum_value=$4
	redeemer_value=$5
	tx_in_collateral=$6
	change_address=$7

	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli transaction build \
			--alonzo-era \
			--tx-in "$tx_in" \
			--tx-in-script-file "$script_file" \
			--tx-in-datum-value "$datum_value" \
			--tx-in-redeemer-value "$redeemer_value" \
			--tx-in-collateral "$tx_in_collateral" \
			--change-address "$change_address" \
			--protocol-params-file "$out_dir"/pparams.json \
			--testnet-magic "${TESTNET_MAGIC}" \
			--out-file "$out_dir"/tx.raw
	else
		edo cardano-cli transaction build \
			--alonzo-era \
			--tx-in "$tx_in" \
			--tx-in-script-file "$script_file" \
			--tx-in-datum-value "$datum_value" \
			--tx-in-redeemer-value "$redeemer_value" \
			--tx-in-collateral "$tx_in_collateral" \
			--change-address "$change_address" \
			--protocol-params-file "$out_dir"/pparams.json \
			--out-file "$out_dir"/tx.raw
	fi

	unset out_dir tx_in script_file datum_value redeemer_value tx_in_collateral change_address
}

# TODO: use wallet API
# @FUNCTION: sign_transaction
# @USAGE: <tx-file> <signing-key> <out-file>
# @DESCRIPTION:
# Sign a transaction.
sign_transaction() {
	[ "$#" -lt 3 ] && die "error: not enough arguments to sign_transaction (expexted 2)"

	edo cardano-cli transaction sign --tx-body-file "$1" --signing-key-file "$2" --out-file "$3"
}

# TODO: use wallet API
# @FUNCTION: submit_transaction
# @USAGE: <signed-tx-file>
# @DESCRIPTION:
# Submit a transaction.
submit_transaction() {
    [ -z "$1" ] && die "Error: no argument given to submit_transaction"

	if [ "$NETWORK" = "testnet" ] ; then
		edo cardano-cli transaction submit --testnet-magic "${TESTNET_MAGIC}" --tx-file "$1"
	else
		edo cardano-cli transaction submit --tx-file "$1"
	fi
}

# @FUNCTION: coin_selection
# @USAGE: <wallet-id> <to-address> <quantity>
# @DESCRIPTION:
# Make a coin selection via the wallet API.
# STDOUT: Response JSON, see https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/selectCoins
coin_selection() {
	[ "$#" -lt 3 ] && die "error: not enough arguments to coin_selection (expexted 3)"

	cat <<EOF > /tmp/payload
{
  "payments": [
    {
      "address": "$2",
      "amount": {
        "quantity": $3,
        "unit": "lovelace"
      }
    }
  ]
}
EOF

	curl -s -H "Content-Type: application/json" --data @/tmp/payload "${WALLET_URL}/v2/wallets/$1/coin-selections/random"

	ret=$?
	rm /tmp/payload

	[ "$ret" -ne 0 ] && die "Failed to perform coin selection"
	unset ret
}




   ####################
   ## High-level API ##
   ####################


# @FUNCTION: bootstrap_wallet
# @USAGE: <memonic> <passphrase>
# @DESCRIPTION:
# Bootstrap the wallet.
# @STDOUT: wallet id
bootstrap_wallet() {
	[ "$#" -lt 2 ] && die "error: not enough arguments to bootstrap_wallet (expexted 2)"

	wallet_id=$(create_wallet "My wallet" "$1" "$2" | jq -r -e .id)
	[ -n "${wallet_id}" ] && [ "${wallet_id}" != "null" ] || die "Could not create wallet"
	edo first_unused_payment_address "$(first_wallet_id)" > out/faucet.addr 2>&1
	if [ "$NETWORK" = "testnet" ] ; then
		(>&2 echo "Apply for faucet funds with the address $(cat out/faucet.addr) at https://testnets.cardano.org/en/testnets/cardano/tools/faucet/"	)
	else
		(>&2 echo "Make sure you have funds to make transactions")
	fi
	(>&2 echo "Press enter to proceed")
	read -r 2>&1

	create_keys out "$1" 2>&1

	script_address out out/result.plutus 2>&1

	(>&2 printf "Your wallet id is: ")
	echo "$wallet_id"
	unset wallet_id
}

# @FUNCTION: burn_funds
# @USAGE: <wallet-id> <commitment> <amount>
# @DESCRIPTION:
# Burn funds.
# @STDOUT: UTxO of the burn script
burn_funds() {
	lock_funds "$1" "$(flip_last_bit "$(sha3_256 "$2")")" "$3"
}

# @FUNCTION: validate_burn
# @USAGE: <cleartext-datum> <script-address>
# @DESCRIPTION:
# Redeem funds.
validate_burn() {
	[ "$#" -lt 2 ] && die "error: not enough arguments to validate_burn (expexted 2)"

	datum=$(datum_hash "$(flip_last_bit "$(sha3_256 "$1")")")
	[ -n "${datum}" ] || die "Could not get datum hash"
	funds=$(get_utxo "$2" | jq --arg h "$datum" -r '[.[] | select(.data_hash ==$h) | .amount | map(.quantity) | map(tonumber)] | flatten | add')

	(>&2 printf "Following amount has been confirmed as burned for the given datum: ")
	echo "$funds"
}

# @FUNCTION: lock_funds
# @USAGE: <wallet-id> <commitment> <amount>
# @DESCRIPTION:
# Burn funds.
# @STDOUT: UTxO of the plutus script
lock_funds() {
	[ "$#" -lt 3 ] && die "error: not enough arguments to lock_funds (expexted 3)"

	edo payment_address out
	datum_hash "$2" > out/burn_hash.txt || die

	json=$(coin_selection "$1" "$(cat out/burn.addr)" "$3")
	[ -n "${json}" ] || die "Could not perform coin selection"
	# TODO: we just pick the first transaction
	tx_hash=$(echo "$json" | jq -e -r '.inputs | .[0].id')
	[ -n "${tx_hash}" ] && [ "${tx_hash}" != "null" ] || die "Could not get TxHash"
	tx_ix=$(echo "$json" | jq -e -r '.inputs | .[0].index')
	[ -n "${tx_ix}" ] && [ "${tx_ix}" != "null" ] || die "Could not get TxIx"
	change_address=$(echo "$json" | jq -e -r '.change | .[0].address')
	[ -n "${change_address}" ] && [ "${change_address}" != "null" ] || die "Could not get change address"

	edo create_script_transaction out "${tx_hash}#${tx_ix}" "$(cat out/burn.addr)" "$3" "$(cat out/burn_hash.txt)" "$(cat out/payment.addr)" "${change_address}"
	edo sign_transaction "out/tx.raw" "out/key.skey" "out/tx.sign"
	edo submit_transaction "out/tx.sign"

	(>&2 echo "Wait a while for the transaction to succeed, then press enter")
	read -r

	get_utxo "$(cat out/burn.addr)"

	rm -f out/burn_hash.txt out/payment.addr tx.raw tx.sign
}

# @FUNCTION: redeem_funds
# @USAGE: <wallet-id> <TxHash> <TxIx> <datum>
# @DESCRIPTION:
# Redeem funds.
redeem_funds() {
	[ "$#" -lt 4 ] && die "error: not enough arguments to redeem_funds (expexted 4)"

	json=$(coin_selection "$1" "$(cat out/burn.addr)" "1000000")
	[ -n "${json}" ] || die "Could not perform coin selection"
	# TODO: we just pick the first transaction
	tx_hash=$(echo "$json" | jq -e -r '.inputs | .[0].id')
	[ -n "${tx_hash}" ] && [ "${tx_hash}" != "null" ] || die "Could not get TxHash"
	tx_ix=$(echo "$json" | jq -e -r '.inputs | .[0].index')
	[ -n "${tx_ix}" ] && [ "${tx_ix}" != "null" ] || die "Could not get TxIx"
	change_address=$(echo "$json" | jq -e -r '.change | .[0].address')
	[ -n "${change_address}" ] && [ "${change_address}" != "null" ] || die "Could not get change address"

	edo redeem_script_transaction \
		out \
		"$2#$3" \
		"out/result.plutus" \
		"$4" \
		"$4" \
		"${tx_hash}#${tx_ix}" \
		"$change_address"
}





   ###################
   ## Start wrapper ##
   ###################

# call arguments verbatim
"$@"
