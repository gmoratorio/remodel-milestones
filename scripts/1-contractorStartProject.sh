#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

getAndSetTestnetMagic

SCRIPT_ADDRESS_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/milestonesScript.addr
SCRIPT_ADDRESS=$(cat $SCRIPT_ADDRESS_FULL_PATH)

section "This script will start the project by updating the UTxO datum to StartProject, without changing any other datum values"

section "First we need to know where all of the wallet files are stored"
read -p "Enter full path to wallets and address files: " PATH_TO_WALLETS

section "Next we need to get the information for the Contractor"
echo "This Contractor address information must match the PubKeyHash provided in DeployOnChain.hs"
echo "For address prompt below, provide the primary address for the Contractor's wallet, with no Stake credential."
echo ""
read -p "Enter Contractor's address filename (i.e. contractor_wallet.addr): " CONTRACTOR_ADDRESS_FILENAME

CONTRACTOR_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$CONTRACTOR_ADDRESS_FILENAME
echo "Contractor's address file is at: " $CONTRACTOR_ADDRESS_FULL_PATH

section "Next we to select a UTxO with enough funds for the collateral and transaction fees"
echo ""
getInputTx ${CONTRACTOR_ADDRESS_FULL_PATH}
COLLATERAL_UTXO=$SELECTED_UTXO

section "Next we to select the UTxO at milestonesScript.addr that is holding the AuthNFT"
echo ""
getInputTx ${SCRIPT_ADDRESS_FULL_PATH}
SCRIPT_AUTH_NFT_UTXO=$SELECTED_UTXO

section "Next we to select the UTxO at the Inspector's address that's holding the Inspector NFT we want to reference"
echo ""
read -p "Enter Inspector's address filename (i.e. inspector_wallet.addr): " INSPECTOR_ADDRESS_FILENAME

INSPECTOR_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$INSPECTOR_ADDRESS_FILENAME
echo "Inspector's address file is at: " $INSPECTOR_ADDRESS_FULL_PATH
getInputTx ${INSPECTOR_ADDRESS_FULL_PATH}
INSPECTOR_REFERENCE_UTXO=$SELECTED_UTXO


section "Finally we need the name of the signing key for the Contractor and their PubKeyHash"
echo "(note the signing key and pkh should be in the same directory as the address file provided earlier)"
read -p "Enter Contractor's signing key that corresponds to the address provided previously (i.e. contractor.skey): " SIGNING_KEY_FILENAME
read -p "Enter the Contractor's pubKeyHash filename (i.e. contractor.pkh): " SIGNER_PUB_KEY_HASH

CONTRACTOR_SIGNING_KEY_FULL_PATH=$PATH_TO_WALLETS/$SIGNING_KEY_FILENAME
CONTRACTOR_PUB_KEY_HASH_FULL_PATH=$PATH_TO_WALLETS/$SIGNER_PUB_KEY_HASH
CONTRACTOR_PHK=$(cat $CONTRACTOR_PUB_KEY_HASH_FULL_PATH)

SCRIPT_FILE=$PATH_TO_MILESTONE_DEPLOY/Milestones.plutus
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
echo "Script address is: " $SCRIPT_ADDRESS

CHANGE_ADDRESS=$(cat $CONTRACTOR_ADDRESS_FULL_PATH)


OLD_DATUM_FILE_NAME=0-parameterized-initial-datum.json
OLD_DATUM_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$OLD_DATUM_FILE_NAME

NEW_DATUM_FILE_NAME=1-deploy-contractor-start-project-datum.json
NEW_DATUM_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$NEW_DATUM_FILE_NAME

REDEEMER_FILE_NAME=redeemer-contractor-start-project.json
REDEEMER_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$REDEEMER_FILE_NAME

TOKEN_QUANTITY=$NFT_QUANTITY_ONE
POLICY_ID=$(cat $PATH_TO_MILESTONE_DEPLOY/policyID)
TOKEN_NAME=$(cat $PATH_TO_MILESTONE_DEPLOY/authTokenName)
TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)

$CARDANO_CLI transaction build \
            --babbage-era \
            --testnet-magic ${TESTNET_MAGIC}  \
            --tx-in ${COLLATERAL_UTXO} \
            --tx-in ${SCRIPT_AUTH_NFT_UTXO} \
            --tx-in-script-file ${SCRIPT_FILE} \
            --tx-in-datum-file ${OLD_DATUM_FILE_FULL_PATH} \
            --tx-in-redeemer-file ${REDEEMER_FILE_FULL_PATH} \
            --read-only-tx-in-reference ${INSPECTOR_REFERENCE_UTXO} \
            --required-signer-hash ${CONTRACTOR_PHK} \
            --tx-in-collateral ${COLLATERAL_UTXO} \
            --tx-out ${SCRIPT_ADDRESS}+${DEFAULT_NFT_MIN_UTXO}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
            --tx-out-datum-embed-file ${NEW_DATUM_FILE_FULL_PATH} \
            --change-address=${CHANGE_ADDRESS} \
            --protocol-params-file $PATH_TO_TRANSACTIONS/pparams.json \
            --out-file $PATH_TO_TRANSACTIONS/tx.draft

$CARDANO_CLI transaction sign \
            --tx-body-file $PATH_TO_TRANSACTIONS/tx.draft \
            --signing-key-file $CONTRACTOR_SIGNING_KEY_FULL_PATH \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $PATH_TO_TRANSACTIONS/tx.signed

$CARDANO_CLI transaction submit --tx-file $PATH_TO_TRANSACTIONS/tx.signed --testnet-magic $TESTNET_MAGIC