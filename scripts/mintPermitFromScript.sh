#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

section "This script will help mint the PermitIssued Inspector NFT"

./mintInspectorNFT.sh "PermitIssued" redeemer-mint-permit.json