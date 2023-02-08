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

section "This script will attempt to deposit additional funds for the Homeowner"

section "First we need to know how much the Homeowner will be depositing."
read -p "Enter total balance (lastBalance) currently at the milestonesScript.addr UTxO holding the AuthNFT [2000000]: " TOTAL_AT_SCRIPT
read -p "Enter amount Homeowner wants to deposit (this must match amount in DeployAdditional_2_HomeownerDeposit.hs [500000000]): " AMOUNT_TO_DEPOSIT
TOTAL_TO_SCRIPT=$(($AMOUNT_TO_DEPOSIT + $TOTAL_AT_SCRIPT))
echo "Total to send back to script: " $TOTAL_TO_SCRIPT

section "Next we need to know where all of the wallet files are stored"
read -p "Enter full path to wallets and address files: " PATH_TO_WALLETS

section "Next we need to get the information for the Homeowner"
echo "This Homeowner address information must match the PubKeyHash provided in DeployOnChain.hs"
echo "For address prompt below, provide the primary address for the Homeowner's wallet, with no Stake credential."
echo ""
read -p "Enter Homeowner's address filename (i.e. homeowner_wallet.addr): " HOMEOWNER_ADDRESS_FILENAME

HOMEOWNER_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$HOMEOWNER_ADDRESS_FILENAME
echo "Homeowner's address file is at: " $HOMEOWNER_ADDRESS_FULL_PATH

section "Next we to select a UTxO with enough funds for the deposit as well as transaction fees"
echo ""
getInputTx ${HOMEOWNER_ADDRESS_FULL_PATH}
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

section "Finally we need the name of the signing key for the Homeowner and their PubKeyHash"
echo "(note the signing key and pkh should be in the same directory as the address file provided earlier)"
read -p "Enter Homeowner's signing key that corresponds to the address provided previously (i.e. homeowner.skey): " SIGNING_KEY_FILENAME
read -p "Enter the Homeowner's pubKeyHash filename (i.e. homeowner.pkh): " SIGNER_PUB_KEY_HASH

HOMEOWNER_SIGNING_KEY_FULL_PATH=$PATH_TO_WALLETS/$SIGNING_KEY_FILENAME
HOMEOWNER_PUB_KEY_HASH_FULL_PATH=$PATH_TO_WALLETS/$SIGNER_PUB_KEY_HASH
HOMEOWNER_PHK=$(cat $HOMEOWNER_PUB_KEY_HASH_FULL_PATH)

CHANGE_ADDRESS=$(cat $HOMEOWNER_ADDRESS_FULL_PATH)

SCRIPT_FILE=$PATH_TO_MILESTONE_DEPLOY/Milestones.plutus

OLD_DATUM_FILE_NAME=1-deploy-contractor-start-project-datum.json
OLD_DATUM_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$OLD_DATUM_FILE_NAME

NEW_DATUM_FILE_NAME=2-deploy-homeowner-deposit-datum.json
NEW_DATUM_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$NEW_DATUM_FILE_NAME

REDEEMER_FILE_NAME=redeemer-homeowner-add-funds.json
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
            --required-signer-hash ${HOMEOWNER_PHK} \
            --tx-in-collateral ${COLLATERAL_UTXO} \
            --tx-out ${SCRIPT_ADDRESS}+${TOTAL_TO_SCRIPT}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
            --tx-out-datum-embed-file ${NEW_DATUM_FILE_FULL_PATH} \
            --change-address=${CHANGE_ADDRESS} \
            --protocol-params-file $PATH_TO_TRANSACTIONS/pparams.json \
            --out-file $PATH_TO_TRANSACTIONS/tx.draft 
            

$CARDANO_CLI transaction sign \
            --tx-body-file $PATH_TO_TRANSACTIONS/tx.draft \
            --signing-key-file $HOMEOWNER_SIGNING_KEY_FULL_PATH \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $PATH_TO_TRANSACTIONS/tx.signed

$CARDANO_CLI transaction submit --tx-file $PATH_TO_TRANSACTIONS/tx.signed --testnet-magic $TESTNET_MAGIC



