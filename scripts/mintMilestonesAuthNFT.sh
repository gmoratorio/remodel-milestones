#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

getAndSetTestnetMagic

section "This script will mint the AuthNFT which will be used to validate all transactions in the Milestones script"

section "First we need the tokenName for the Milestones AuthNFT (must match the tokenName entered in DeployNFT.hs)"
read -p "Enter tokenName for AuthNFT: " TOKEN_NAME

SCRIPT_FILE=$PATH_TO_MILESTONE_DEPLOY/Mint.plutus
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)

POLICY_ID=$($CARDANO_CLI transaction policyid --script-file $SCRIPT_FILE)

read -p "Enter full path to wallets and address files: " PATH_TO_WALLETS

section "Next we need to get the information for the Contractor"
echo "This Contractor address information must match the PubKeyHash provided in DeployNFT.hs"
echo "For address prompt below, provide the primary address for the Contractor's wallet, with no Stake credential."
echo ""
read -p "Enter Contractor's address filename (i.e. contractor_wallet.addr): " CONTRACTOR_ADDRESS_FILENAME

CONTRACTOR_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$CONTRACTOR_ADDRESS_FILENAME
echo "Contractor's address file is at: " $CONTRACTOR_ADDRESS_FULL_PATH


section "Now we need the UTxO used to create unique AuthNFT PolicyID (must match UTxO used in DeployNFT.hs)"
echo ""
getInputTx ${CONTRACTOR_ADDRESS_FULL_PATH}
COLLATERAL_TX=$SELECTED_UTXO

FROM_WALLET_ADDRESS=$(cat $CONTRACTOR_ADDRESS_FULL_PATH)
TO_WALLET_ADDRESS=$(cat $CONTRACTOR_ADDRESS_FULL_PATH)
CHANGE_ADDRESS=$(cat $CONTRACTOR_ADDRESS_FULL_PATH)

TOKEN_QUANTITY=$NFT_QUANTITY_ONE

TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)

REDEEMER_FILE_FULL_PATH=$PATH_TO_MILESTONE_DEPLOY/redeemer-mint.json

section "Finally we need the name of the signing key for the Contractor and their PubKeyHash"
echo "(note the signing key and pkh should be in the same directory as the address file provided earlier)"
read -p "Enter Contractor's signing key that corresponds to the address provided previously (i.e. contrator.skey): " SIGNING_KEY_FILENAME
read -p "Enter the Contractor's pubKeyHash filename (i.e. contractor.pkh): " SIGNER_PUB_KEY_HASH

CONTRACTOR_SIGNING_KEY_FULL_PATH=$PATH_TO_WALLETS/$SIGNING_KEY_FILENAME
CONTRACTOR_PUB_KEY_HASH_FULL_PATH=$PATH_TO_WALLETS/$SIGNER_PUB_KEY_HASH
CONTRACTOR_PHK=$(cat $CONTRACTOR_PUB_KEY_HASH_FULL_PATH)

$CARDANO_CLI transaction build \
--babbage-era \
--cardano-mode \
--testnet-magic $TESTNET_MAGIC \
--tx-in ${COLLATERAL_TX} \
--tx-out ${TO_WALLET_ADDRESS}+${DEFAULT_NFT_MIN_UTXO}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--change-address ${CHANGE_ADDRESS} \
--mint "$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--mint-script-file ${SCRIPT_FILE} \
--mint-redeemer-file ${REDEEMER_FILE_FULL_PATH} \
--required-signer-hash ${CONTRACTOR_PHK} \
--tx-in-collateral ${COLLATERAL_TX} \
--protocol-params-file $PATH_TO_TRANSACTIONS/pparams.json \
--out-file $PATH_TO_TRANSACTIONS/tx.draft

$CARDANO_CLI transaction sign \
--tx-body-file $PATH_TO_TRANSACTIONS/tx.draft \
--signing-key-file $CONTRACTOR_SIGNING_KEY_FULL_PATH \
--testnet-magic $TESTNET_MAGIC \
--out-file $PATH_TO_TRANSACTIONS/tx.signed \

$CARDANO_CLI transaction submit --tx-file $PATH_TO_TRANSACTIONS/tx.signed --testnet-magic $TESTNET_MAGIC

POLICY_ID_FILE=$PATH_TO_MILESTONE_DEPLOY/policyID
echo $POLICY_ID > $POLICY_ID_FILE
section "Your AuthNFT has been created"
echo "The policyID for this AuthNFT is: " $POLICY_ID
echo "The policyID for this AuthNFT is located at /Milestones/Deploy/policyID"
echo "Use this policyID for the Milestones deployment in Milestones.DeployOnChain.hs"
echo "Additionally, this policyID will automatically be used by all other shell scripts interacting with this validator"
echo ""

AUTH_TOKEN_NAME_FILE=$PATH_TO_MILESTONE_DEPLOY/authTokenName
echo $TOKEN_NAME > $AUTH_TOKEN_NAME_FILE
echo "The TokenName for the AuthNFT is located at /Milestones/Deploy/authTokenName"
echo "This authTokenName will automatically be used by all other shell scripts interacting with this validator"