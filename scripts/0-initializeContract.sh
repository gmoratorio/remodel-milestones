#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

getAndSetTestnetMagic

section "This script will initialize the Milestones contract by creating & sending the NotStarted UTxO datum with default values"

section "First we need to know where all of the wallet files are stored"
read -p "Enter full path to wallets and address files: " PATH_TO_WALLETS

section "Next we need to get the information for the Contractor"
echo "This Contractor address information must match the PubKeyHash provided in DeployOnChain.hs"
echo "For address prompt below, provide the primary address for the Contractor's wallet, with no Stake credential."
echo ""
read -p "Enter Contractor's address filename (i.e. contractor_wallet.addr): " CONTRACTOR_ADDRESS_FILENAME

CONTRACTOR_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$CONTRACTOR_ADDRESS_FILENAME
echo "Contractor's address file is at: " $CONTRACTOR_ADDRESS_FULL_PATH

section "Now we need the Contractor's UTxO holding the AuthNFT (must match the policyID created from mintMilestonesNFT.sh)"
echo ""
getInputTx ${CONTRACTOR_ADDRESS_FULL_PATH}
AUTH_NFT_UTXO=$SELECTED_UTXO

section "Next we a collateral & fees UTxO for the Contractor to send along with the authNFT"
echo ""
getInputTx ${CONTRACTOR_ADDRESS_FULL_PATH}
COLLATERAL_UTXO=$SELECTED_UTXO

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

DATUM_FILE_NAME=0-parameterized-initial-datum.json
DATUM_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/$DATUM_FILE_NAME

TOKEN_QUANTITY=$NFT_QUANTITY_ONE
POLICY_ID=$(cat $PATH_TO_MILESTONE_DEPLOY/policyID)
TOKEN_NAME=$(cat $PATH_TO_MILESTONE_DEPLOY/authTokenName)
TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)

$CARDANO_CLI transaction build \
            --tx-in ${COLLATERAL_UTXO} \
            --tx-in ${AUTH_NFT_UTXO} \
            --tx-out ${SCRIPT_ADDRESS}+${DEFAULT_NFT_MIN_UTXO}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
            --required-signer-hash ${CONTRACTOR_PHK} \
            --tx-out-datum-embed-file ${DATUM_FILE_FULL_PATH} \
            --change-address=${CHANGE_ADDRESS} \
            --testnet-magic ${TESTNET_MAGIC}  \
            --out-file $PATH_TO_TRANSACTIONS/tx.draft \
            --protocol-params-file $PATH_TO_TRANSACTIONS/pparams.json \
            --babbage-era

$CARDANO_CLI transaction sign \
            --tx-body-file $PATH_TO_TRANSACTIONS/tx.draft \
            --signing-key-file $CONTRACTOR_SIGNING_KEY_FULL_PATH \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $PATH_TO_TRANSACTIONS/tx.signed

$CARDANO_CLI transaction submit --tx-file $PATH_TO_TRANSACTIONS/tx.signed --testnet-magic $TESTNET_MAGIC

SCRIPT_ADDRESS_FILE=$PATH_TO_MILESTONE_DEPLOY/milestonesScript.addr
echo $SCRIPT_ADDRESS > $SCRIPT_ADDRESS_FILE
section "Your Milestones AuthNFT has been deployed to the Milestones script address on chain"
echo "The Script Address for this validator is located at /Milestones/Deploy/milestonesScript.addr"
echo "This milestonesScript.addr will automatically be used by all other shell scripts interacting with this validator"