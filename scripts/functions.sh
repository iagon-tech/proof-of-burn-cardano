#!/bin/sh

# @FUNCTION: to_hex
# @USAGE: <ascii-string>
# @DESCRIPTION:
# Converts an ascii string to uppercase hex string (no leading  '0x').
# @STDOUT: The converted to hex string
to_hex() {
	xxd -p -u <<EOF
$1
EOF
}

# @FUNCTION: from_hex
# @USAGE: <hex-string>
# @DESCRIPTION:
# Converts a hex string to an ascii string.
# @STDOUT: The converted to ascii string
from_hex() {
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
	printf '%X\n' "$(( "$1" ^ 1 ))"
}

# @FUNCTION: sha3_256
# @USAGE: <string>
# @DESCRIPTION:
# Hash a string with sha3-256 in hex format.
# @STDOUT: The hashed string.
sha3_256() {
	printf "%s" "$1" | openssl dgst -r -sha3-256 | awk '{ print $1 }'
}

# @FUNCTION: datum_hash
# @USAGE: <string>
# @DESCRIPTION:
# Get the datum hash of a string in hex format.
# @STDOUT: The hashed string.
datum_hash() {
	cardano-cli transaction hash-script-data --script-data-value "$1"
}

# @FUNCTION: to_burn_datum_hash
# @USAGE: <string>
# @DESCRIPTION:
# Converts burn commitment to a full datum hash ready to be submitted
# in a transaction.
# @STDOUT: The hashed string.
to_burn_datum_hash(){
	datum_hash "$(flip_last_bit "$(sha3_256 "$1")")"
}
