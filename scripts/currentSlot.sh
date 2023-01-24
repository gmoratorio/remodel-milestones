#!/usr/bin/env bash
set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh

# $1 first arg is TESTNET_MAGIC
TESTNET_MAGIC=$1

$CARDANO_CLI query tip --testnet-magic $TESTNET_MAGIC | jq -r '.slot'