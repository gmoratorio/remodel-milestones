#!/usr/bin/env bash

set -e
set -o pipefail

# relative paths for scripts
export PATH_TO_INSPECTOR_DEPLOY=../src/Inspector/Deploy
export PATH_TO_MILESTONE_DEPLOY=../src/Milestones/Deploy
export PATH_TO_TRANSACTIONS=../transactions
export DEFAULT_ADA_COLLATERAL=2000000
export NFT_QUANTITY_ONE=1

export CARDANO_CLI=cardano-cli
