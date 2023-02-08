#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

section "This script will help mint the RoughPassed Inspector NFT"

./mintInspectorNFT.sh "RoughPassed" redeemer-mint-rough.json