#!/usr/bin/env bash
set -e
set -o pipefail

# uncomment below for script entry debugging
# set -x

source env.sh

getAndSetTestnetMagic() {
	read -p 'Testnet magic number: 1 for preprod (default), 2 for preview ' TESTNET_MAGIC_STRING

	if [[ $TESTNET_MAGIC_STRING == "2" ]];
	then 
		TESTNET_MAGIC=2
		NETWORK_NAME="preview"
	else
		TESTNET_MAGIC=1
		NETWORK_NAME="preprod"
	fi

	echo 'testnet-magic is' $TESTNET_MAGIC
	echo 'network name is' $NETWORK_NAME
}

function getInputTx() {
    # $1 argument 1 is INSPECTOR_ADDRESS_FULL_PATH
    PATH_TO_SELECTED_WALLET_ADDRESS=$1

	BALANCE_FILE=$PATH_TO_TRANSACTIONS/walletBalances.txt
	rm -f $BALANCE_FILE
	
	./balance.sh $PATH_TO_SELECTED_WALLET_ADDRESS $TESTNET_MAGIC > $BALANCE_FILE

	cat $BALANCE_FILE
	read -p 'TX row number starting in 1: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
}

function section {
  echo ""
  echo "============================================================================================"
  echo $1
  echo "============================================================================================"
}