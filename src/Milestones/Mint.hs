{-# LANGUAGE NoImplicitPrelude #-}

module Milestones.Mint where

import           PlutusTx.Prelude
import qualified Ledger                              as Ledger
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1


data MilestoneMintParam = MilestoneMintParam 
                { utxo ::           LedgerApiV2.TxOutRef 
                , tokenName ::      LedgerApiV2.TokenName
                , contractor ::     Ledger.PaymentPubKeyHash
                , deadline ::       LedgerApiV2.POSIXTime
                }

PlutusTx.unstableMakeIsData ''MilestoneMintParam
PlutusTx.makeLift ''MilestoneMintParam


{-# INLINABLE mkPolicy #-}
mkPolicy :: MilestoneMintParam -> () -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy param _ context =
    let 
        txinfo :: LedgerContextsV2.TxInfo
        txinfo = LedgerContextsV2.scriptContextTxInfo context

        outputs :: [LedgerContextsV2.TxOut]
        outputs = LedgerContextsV2.txInfoOutputs txinfo
        
        contractorAddress :: Ledger.Address
        contractorAddress = Ledger.pubKeyHashAddress (contractor param) Nothing

        sentToContractorAddress :: Bool
        sentToContractorAddress = all (\txOut -> (LedgerContextsV2.txOutAddress txOut) == contractorAddress) outputs

        signedByContractor :: Bool
        signedByContractor = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash $ contractor param

        getTokenNameAndAmount :: Maybe (LedgerApiV2.TokenName, Integer)
        getTokenNameAndAmount = case LedgerValueV1.flattenValue $ LedgerContextsV2.txInfoMint txinfo of
                        [(_, tn, amount)]   -> Just (tn, amount)
                        _                   -> Nothing

        correctNFTAndAmount :: LedgerApiV2.TokenName -> (Bool, Bool)
        correctNFTAndAmount tokenName = do
            case getTokenNameAndAmount of
                Just (tn, amt)  -> (tn == tokenName, amt == 1)
                Nothing         -> (False, False)

        beforeDeadline :: Bool
        beforeDeadline = (deadline param) `LedgerIntervalV1.after` (LedgerContextsV2.txInfoValidRange txinfo) 

        hasPolicyUtxo :: Bool
        hasPolicyUtxo = any (\txInInfo -> LedgerContextsV2.txInInfoOutRef txInInfo == utxo param) $ LedgerContextsV2.txInfoInputs txinfo

        (correctNFT, correctAmount) = correctNFTAndAmount $ tokenName param

    in
        traceIfFalse "Deadline has passed" beforeDeadline &&
        traceIfFalse "Policy Utxo not provided" hasPolicyUtxo &&
        traceIfFalse "Not signed by inspector" signedByContractor &&
        traceIfFalse "Wrong token name" correctNFT &&
        traceIfFalse "Wrong amount" correctAmount &&
        traceIfFalse "Not sent back to inspector's address" sentToContractorAddress

wrappedPolicy :: MilestoneMintParam -> BuiltinData -> BuiltinData -> ()
wrappedPolicy param r sc = check (mkPolicy param (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData sc))

policy :: MilestoneMintParam -> LedgerApiV2.MintingPolicy
policy milestoneParam = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrappedPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode milestoneParam

currencySymbol :: MilestoneMintParam -> Ledger.CurrencySymbol
currencySymbol = UtilsScriptsV2.scriptCurrencySymbol . policy
