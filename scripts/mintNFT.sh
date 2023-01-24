#!/usr/bin/env bash

set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh
source helpers.sh

getAndSetTestnetMagic

# $1 arg is TOKEN_NAME
# $2 arg is REDEEMER_FILE_NAME

TOKEN_NAME=$1
REDEEMER_FILE_NAME=$2

SCRIPT_FILE=$PATH_TO_DEPLOY/Mint.plutus
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)

POLICY_ID=$($CARDANO_CLI transaction policyid --script-file $SCRIPT_FILE)

read -p "Enter full path to wallets and address files: " PATH_TO_WALLETS

section "Next we need to get the information for the Inspector"
echo "This Inspector address information must match the PubKeyHash provided in Deploy.hs"
echo "For address prompt below, provide the primary address for the Inspector's wallet, with no Stake credential."
echo ""
read -p "Enter Inspector's address filename (i.e. inspector_wallet.addr): " INSPECTOR_ADDRESS_FILENAME

INSPECTOR_ADDRESS_FULL_PATH=$PATH_TO_WALLETS/$INSPECTOR_ADDRESS_FILENAME
echo "Inspector's address file is at: " $INSPECTOR_ADDRESS_FULL_PATH

if [[ $TOKEN_NAME == "PermitIssued" ]]
then
    section "Now we need the UTxO used to create unique NFT PolicyID (must match UTxO used in Deploy.hs)"
else 
    section "Now we need to select a collateral UTxO"
fi
echo ""
getInputTx ${INSPECTOR_ADDRESS_FULL_PATH}
COLLATERAL_TX=$SELECTED_UTXO

FROM_WALLET_ADDRESS=$(cat $INSPECTOR_ADDRESS_FULL_PATH)
TO_WALLET_ADDRESS=$(cat $INSPECTOR_ADDRESS_FULL_PATH)
CHANGE_ADDRESS=$(cat $INSPECTOR_ADDRESS_FULL_PATH)

TOKEN_QUANTITY=$NFT_QUANTITY_ONE

TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)

REDEEMER_FILE_FULL_PATH=$PATH_TO_DEPLOY/$REDEEMER_FILE_NAME

section "Finally we need the name of the signing key for the Inspector and their PubKeyHash"
echo "(note the signing key and pkh should be in the same directory as the address file provided earlier)"
read -p "Enter Inspector's signing key that corresponds to the address provided previously (i.e. inspector.skey): " SIGNING_KEY_FILENAME
read -p "Enter the Inspector's pubKeyHash filename (i.e. inspector.pkh): " SIGNER_PUB_KEY_HASH

INSPECTOR_SIGNING_KEY_FULL_PATH=$PATH_TO_WALLETS/$SIGNING_KEY_FILENAME
INSPECTOR_PUB_KEY_HASH_FULL_PATH=$PATH_TO_WALLETS/$SIGNER_PUB_KEY_HASH
INSPECTOR_PHK=$(cat $INSPECTOR_PUB_KEY_HASH_FULL_PATH)

$CARDANO_CLI transaction build \
--babbage-era \
--cardano-mode \
--testnet-magic $TESTNET_MAGIC \
--tx-in ${COLLATERAL_TX} \
--tx-out ${TO_WALLET_ADDRESS}+${DEFAULT_ADA_COLLATERAL}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--change-address ${CHANGE_ADDRESS} \
--mint "$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--mint-script-file ${SCRIPT_FILE} \
--mint-redeemer-file ${REDEEMER_FILE_FULL_PATH} \
--required-signer-hash ${INSPECTOR_PHK} \
--tx-in-collateral ${COLLATERAL_TX} \
--protocol-params-file $PATH_TO_TRANSACTIONS/pparams.json \
--out-file $PATH_TO_TRANSACTIONS/tx.draft

$CARDANO_CLI transaction sign \
--tx-body-file $PATH_TO_TRANSACTIONS/tx.draft \
--signing-key-file $INSPECTOR_SIGNING_KEY_FULL_PATH \
--testnet-magic $TESTNET_MAGIC \
--out-file $PATH_TO_TRANSACTIONS/tx.signed \

$CARDANO_CLI transaction submit --tx-file $PATH_TO_TRANSACTIONS/tx.signed --testnet-magic $TESTNET_MAGIC