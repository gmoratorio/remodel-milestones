#!/usr/bin/env bash
set -e
set -o pipefail

source env.sh

# $1 is PATH_TO_SELECTED_WALLET_ADDRESS
# $2 is TESTNET_MAGIC

$CARDANO_CLI query utxo --address $(cat $1) --testnet-magic $2


