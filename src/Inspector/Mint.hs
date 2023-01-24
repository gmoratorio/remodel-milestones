{-# LANGUAGE NoImplicitPrelude #-}


module Inspector.Mint where

import           PlutusTx.Prelude
import qualified Ledger                              as Ledger
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2


data PermitParam = PermitParam 
            { utxo ::           LedgerApiV2.TxOutRef
            , inspector ::      Ledger.PaymentPubKeyHash
            , permitName ::     LedgerApiV2.TokenName
            , roughName ::      LedgerApiV2.TokenName
            , drywallName ::    LedgerApiV2.TokenName
            , finalName ::      LedgerApiV2.TokenName
            , closedName ::     LedgerApiV2.TokenName
            }

PlutusTx.unstableMakeIsData ''PermitParam
PlutusTx.makeLift ''PermitParam

data Action = PermitIssued | RoughPassed | DrywallPassed | FinalPassed | ClosedIncomplete
PlutusTx.unstableMakeIsData ''Action

{-# INLINABLE mkPolicy #-}
mkPolicy :: PermitParam -> Action -> LedgerContextsV2.ScriptContext -> Bool
mkPolicy param action context =
    let 
        txinfo :: LedgerContextsV2.TxInfo
        txinfo = LedgerContextsV2.scriptContextTxInfo context

        outputs :: [LedgerContextsV2.TxOut]
        outputs = LedgerContextsV2.txInfoOutputs txinfo
        
        inspectorAddress :: Ledger.Address
        inspectorAddress = Ledger.pubKeyHashAddress (inspector param) Nothing

        sentToInspectorAddress :: Bool
        sentToInspectorAddress = 
                let onlyOneOutput = length outputs == 1
                    firstOutput = head outputs
                    sentToInspector = (LedgerContextsV2.txOutAddress firstOutput) == inspectorAddress

                in onlyOneOutput && sentToInspector

        signedByInspector :: Bool
        signedByInspector = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash $ inspector param

        getTokenNameAndAmount :: Maybe (LedgerApiV2.TokenName, Integer)
        getTokenNameAndAmount = case LedgerValueV1.flattenValue $ LedgerContextsV2.txInfoMint txinfo of
                        [(_, tn, amount)]   -> Just (tn, amount)
                        _                   -> Nothing

        correctNFTAndAmount :: LedgerApiV2.TokenName -> (Bool, Bool)
        correctNFTAndAmount tokenActionName = do
            case getTokenNameAndAmount of
                Just (tn, amt)  -> (tn == tokenActionName, amt == 1)
                Nothing         -> (False, False)

    in
        case action of 
            PermitIssued -> 
                let hasPolicyUtxo :: Bool
                    hasPolicyUtxo = any (\txInInfo -> LedgerContextsV2.txInInfoOutRef txInInfo == utxo param) $ LedgerContextsV2.txInfoInputs txinfo

                    (correctNFT, correctAmount) = correctNFTAndAmount $ permitName param

                in
                    traceIfFalse "Policy Utxo not provided" hasPolicyUtxo &&
                    traceIfFalse "Not signed by inspector" signedByInspector &&
                    traceIfFalse "Wrong token name" correctNFT &&
                    traceIfFalse "Wrong amount" correctAmount &&
                    traceIfFalse "Not sent back to inspector's address" sentToInspectorAddress
            
            RoughPassed ->
                let (correctNFT, correctAmount) = correctNFTAndAmount $ roughName param

                in
                    traceIfFalse "Not signed by inspector" signedByInspector &&
                    traceIfFalse "Wrong token name" correctNFT &&
                    traceIfFalse "Wrong amount" correctAmount &&
                    traceIfFalse "Not sent back to inspector's address" sentToInspectorAddress
            
            DrywallPassed -> 
                let (correctNFT, correctAmount) = correctNFTAndAmount $ drywallName param

                in
                    traceIfFalse "Not signed by inspector" signedByInspector &&
                    traceIfFalse "Wrong token name" correctNFT &&
                    traceIfFalse "Wrong amount" correctAmount &&
                    traceIfFalse "Not sent back to inspector's address" sentToInspectorAddress

            FinalPassed -> 
                let (correctNFT, correctAmount) = correctNFTAndAmount $ finalName param

                in
                    traceIfFalse "Not signed by inspector" signedByInspector &&
                    traceIfFalse "Wrong token name" correctNFT &&
                    traceIfFalse "Wrong amount" correctAmount &&
                    traceIfFalse "Not sent back to inspector's address" sentToInspectorAddress

            ClosedIncomplete -> 
                let (correctNFT, correctAmount) = correctNFTAndAmount $ closedName param

                in
                    traceIfFalse "Not signed by inspector" signedByInspector &&
                    traceIfFalse "Wrong token name" correctNFT &&
                    traceIfFalse "Wrong amount" correctAmount &&
                    traceIfFalse "Not sent back to inspector's address" sentToInspectorAddress

wrappedPolicy :: PermitParam -> BuiltinData -> BuiltinData -> ()
wrappedPolicy param r sc = check (mkPolicy param (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData sc))

policy :: PermitParam -> LedgerApiV2.MintingPolicy
policy permitParam = LedgerApiV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrappedPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode permitParam

currencySymbol :: PermitParam -> Ledger.CurrencySymbol
currencySymbol = UtilsScriptsV2.scriptCurrencySymbol . policy
