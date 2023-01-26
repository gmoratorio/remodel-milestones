module Milestones.OnChain where

import qualified Prelude                            as P
import Data.Maybe                                   (fromJust)
import PlutusTx.Prelude                   
import qualified Ledger                             as Ledger
import qualified Ledger.Ada                         as Ada
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Contexts          as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value             as LedgerValueV1
import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import qualified Plutus.V2.Ledger.Tx                as PlutusV2LedgerTx
import qualified Plutus.V1.Ledger.Scripts           as ScriptsLedger

data MilestoneParam = MilestoneParam 
                { homeowner ::          Ledger.PaymentPubKeyHash
                , contractor ::         Ledger.PaymentPubKeyHash
                , totalCost ::          Integer
                , projectPolicyId ::    BuiltinByteString
                , inspectionPolicyId :: BuiltinByteString
                , permitName ::         LedgerApiV2.TokenName
                , roughName ::          LedgerApiV2.TokenName
                , drywallName ::        LedgerApiV2.TokenName
                , finalName ::          LedgerApiV2.TokenName
                , closedName ::         LedgerApiV2.TokenName
                }

PlutusTx.unstableMakeIsData ''MilestoneParam
PlutusTx.makeLift ''MilestoneParam      

data RedeemHomeowner =  HomeownerAddFunds | HomeownerWithdrawFunds deriving (P.Eq, Eq, P.Ord)

PlutusTx.unstableMakeIsData ''RedeemHomeowner
PlutusTx.makeLift ''RedeemHomeowner

data RedeemContractor = StartProject |
                        WithdrawPermitPayment | 
                        WithdrawRoughPayment | 
                        WithdrawDrywallPayment | 
                        WithdrawFinalPayment
                            deriving (P.Eq, Eq, P.Ord)

PlutusTx.unstableMakeIsData ''RedeemContractor
PlutusTx.makeLift ''RedeemContractor

data MilestoneRedeem = Homeowner RedeemHomeowner | Contractor RedeemContractor deriving (P.Eq)

PlutusTx.unstableMakeIsData ''MilestoneRedeem
PlutusTx.makeLift ''MilestoneRedeem

data MilestoneDatum = MilestoneDatum
                { lastContractorAction :: RedeemContractor
                , totalDeposited :: Integer
                , lastBalance :: Integer
                , totalWithdrawnContractor :: Integer
                , totalWithdrawnHomeowner :: Integer
                } deriving (P.Eq, Eq)

PlutusTx.unstableMakeIsData ''MilestoneDatum
PlutusTx.makeLift ''MilestoneDatum

{-# INLINABLE milestoneValidator #-}
milestoneValidator :: MilestoneParam -> MilestoneDatum -> MilestoneRedeem -> LedgerContextsV2.ScriptContext -> Bool
milestoneValidator param dat redeem sc =
    let
        txinfo :: LedgerContextsV2.TxInfo
        txinfo = LedgerContextsV2.scriptContextTxInfo sc

        -- inputs we're consuming
        inputs :: [LedgerContextsV2.TxInInfo]
        inputs = LedgerContextsV2.txInfoInputs txinfo

        -- inputs we're only referencing
        referenceInputs :: [LedgerContextsV2.TxInInfo]
        referenceInputs = LedgerContextsV2.txInfoReferenceInputs txinfo

        -- all outputs
        outputs :: [LedgerContextsV2.TxOut]
        outputs = LedgerContextsV2.txInfoOutputs txinfo

        -- outputs going back to the script they're spent from
        continuingOutputs :: [LedgerContextsV2.TxOut]
        continuingOutputs = LedgerContextsV2.getContinuingOutputs sc

        threadPolicyId :: LedgerApiV2.CurrencySymbol
        threadPolicyId =  LedgerApiV2.CurrencySymbol (projectPolicyId param)

        inspectorPolicyId :: LedgerApiV2.CurrencySymbol
        inspectorPolicyId =  LedgerApiV2.CurrencySymbol (inspectionPolicyId param)

        getInputValue :: LedgerContextsV2.TxInInfo -> LedgerValueV1.Value
        getInputValue = LedgerContextsV2.txOutValue . LedgerContextsV2.txInInfoResolved

        valueContainsThreadPolicy :: LedgerValueV1.Value -> Bool
        valueContainsThreadPolicy value =
                let currencies = LedgerValueV1.flattenValue $ value
                in any(\(policyId, _, _) -> policyId == threadPolicyId) currencies

        threadNFTBackToScript :: Bool
        threadNFTBackToScript = isJust $ find (\txout -> valueContainsThreadPolicy $ LedgerContextsV2.txOutValue txout) continuingOutputs

        inputHasThreadNFT :: Bool
        inputHasThreadNFT = 
            let maybeThreadTx = LedgerContextsV2.findOwnInput sc
            in 
                case maybeThreadTx of
                    Just (txInInfo) -> valueContainsThreadPolicy $ getInputValue txInInfo
                    _ -> False

        exactlyOneReferenceInput :: Bool
        exactlyOneReferenceInput = length referenceInputs == 1

        exactlyOneContinuingOutput :: Bool
        exactlyOneContinuingOutput = length continuingOutputs == 1
    in
        traceIfFalse "All transactions must include thread NFT for authentication" inputHasThreadNFT &&
        traceIfFalse "All transactions must send thread NFT back to script" threadNFTBackToScript &&
        traceIfFalse "Only one reference input can be provided at a time" exactlyOneReferenceInput &&
        traceIfFalse "Only one UTxO can be sent back to the script at a time" exactlyOneContinuingOutput && 

        let
            maybeInspectorReferenceToken :: Maybe LedgerApiV2.TokenName
            maybeInspectorReferenceToken = 
                -- at this point we know we have exactly one referenceInput
                let firstInput = head referenceInputs
                    flattenedInspectorValue = filter (\(curSymbol, _, _) -> curSymbol == inspectorPolicyId) $ LedgerValueV1.flattenValue $ getInputValue firstInput
                in 
                    case flattenedInspectorValue of
                        [(_, inspectorTokenName, _)]    -> Just (inspectorTokenName)
                        _                               -> Nothing

            includesInspectorReferenceToken :: Bool
            includesInspectorReferenceToken = isJust maybeInspectorReferenceToken
        in 
            traceIfFalse "All transactions must include reference to Inspector NFT token" includesInspectorReferenceToken &&
            let
                -- at this point we're sure that we have a real inspectorReferenceToken
                inspectorReferenceToken :: LedgerApiV2.TokenName
                inspectorReferenceToken = fromJust maybeInspectorReferenceToken

                -- at this point we're sure that we only have 1 output back to the script
                outputToScript :: LedgerContextsV2.TxOut
                outputToScript = head continuingOutputs

                continuingOutValue :: LedgerValueV1.Value
                continuingOutValue = LedgerContextsV2.txOutValue outputToScript

                adaToScript :: Integer
                adaToScript = LedgerValueV1.valueOf continuingOutValue LedgerApiV2.adaSymbol LedgerApiV2.adaToken

                outDatum :: MilestoneDatum
                outDatum = case PlutusV2LedgerTx.txOutDatum outputToScript of
                                PlutusV2LedgerTx.NoOutputDatum -> traceError "No datum in outputToScript"
                                PlutusV2LedgerTx.OutputDatumHash _ -> traceError "Datum hash, not full Datum found"
                                PlutusV2LedgerTx.OutputDatum d -> 
                                    case PlutusTx.fromBuiltinData $ ScriptsLedger.getDatum d of
                                        Nothing -> traceError "Error converting outDatum to MilestoneDatum"
                                        Just (outDat) -> outDat

                isWithdrawal :: Bool
                isWithdrawal = adaToScript < totalDeposited dat

                isDeposit :: Bool
                isDeposit = adaToScript > totalDeposited dat

                datumActionMatchesRedeem :: RedeemContractor -> Bool
                datumActionMatchesRedeem = ((lastContractorAction outDatum) ==)

                confirmedByInspector :: LedgerApiV2.TokenName -> Bool
                confirmedByInspector = (inspectorReferenceToken ==)

                previousContractorAction :: RedeemContractor
                previousContractorAction = lastContractorAction dat

                nextContractorAction :: RedeemContractor
                nextContractorAction = lastContractorAction outDatum

                projectCost :: Integer
                projectCost = totalCost param

                signedByContractor :: Bool
                signedByContractor = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (contractor param)

                signedByHomeowner :: Bool
                signedByHomeowner = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (homeowner param)

                intToAda :: Integer -> Ada.Ada
                intToAda = Ada.lovelaceOf . (*1000000)

                scheduledPayout :: Ada.Ada
                scheduledPayout = 
                    case redeem of
                        Contractor (WithdrawPermitPayment)   -> intToAda $ projectCost `P.div` 4
                        Contractor (WithdrawRoughPayment)    -> intToAda $ projectCost `P.div` 4
                        Contractor (WithdrawDrywallPayment)  -> intToAda $ projectCost `P.div` 4
                        -- if total doesn't divide evenly into 4 payments, pay the remainder with the final payment
                        Contractor (WithdrawFinalPayment)    -> intToAda $ (projectCost `P.div` 4) + (projectCost `P.rem` 4)
                        _                                    -> intToAda 0
 
                contractorPayoutAmount :: Ada.Ada
                contractorPayoutAmount = Ada.fromValue $ LedgerContextsV2.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (contractor param))

                homeownerWithrawalAmount :: Ada.Ada
                homeownerWithrawalAmount = Ada.fromValue $ LedgerContextsV2.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (homeowner param))

            in
                traceIfFalse "outDatum lastBalance must match adaToScript" ((lastBalance outDatum) == adaToScript) &&

                case redeem of 
                    Homeowner (_) ->
                        traceIfFalse "Must be signed by homeowner" signedByHomeowner &&

                        case redeem of
                            Homeowner (HomeownerAddFunds) ->
                                traceIfTrue "Cannot add any more funds if project is ClosedIncomplete" (confirmedByInspector (closedName param)) &&
                                traceIfTrue "Cannot add any more funds if totalCost was previously met" (totalDeposited dat >= projectCost) && 
                                traceIfTrue "Deposit would pass project's totalCost" (totalDeposited outDatum >= totalCost param) &&
                                traceIfFalse "No withdrawal allowed at this step" (Ada.isZero homeownerWithrawalAmount) &&
                                traceIfFalse "This is a deposit. totalDeposited to script must go up!" isDeposit &&
                                traceIfFalse "totalDeposited increase must match lastBalance increase" 
                                                            (((totalDeposited outDatum) - (totalDeposited dat)) == ((lastBalance outDatum) - (lastBalance dat))) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                previousContractorAction == nextContractorAction && 
                                                                (totalWithdrawnContractor dat) == (totalWithdrawnContractor outDatum) && 
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum)
                                                                )

                            Homeowner (HomeownerWithdrawFunds) ->
                                traceIfFalse "Homeowner cannot withdraw funds unless project is ClosedIncomplete" (confirmedByInspector (closedName param)) &&
                                traceIfFalse "totalWithdrawnHomeowner increase must match lastBalance decrease" 
                                                            (((totalWithdrawnHomeowner outDatum) - (totalWithdrawnHomeowner dat)) == ((lastBalance dat) - (lastBalance outDatum))) &&
                                traceIfFalse "This is a withdrawal. adaToScript must go down!" isWithdrawal &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                (previousContractorAction == nextContractorAction) && 
                                                (totalWithdrawnContractor dat) == (totalWithdrawnContractor outDatum) && 
                                                (totalDeposited dat) == (totalDeposited outDatum)
                                                )
                    
                    Contractor (_) ->
                        traceIfFalse "Must be signed by contractor" signedByContractor &&
                        traceIfFalse "Contractor payout must match schedule for this milestone" (contractorPayoutAmount == scheduledPayout) &&
                        traceIfFalse "totalWithdrawnContractor increase must match lastBalance decrease" 
                             (((totalWithdrawnContractor outDatum) - (totalWithdrawnContractor dat)) == ((lastBalance dat) - (lastBalance outDatum))) &&

                        case redeem of
                            Contractor (StartProject) ->
                                traceIfFalse "No withdrawal allowed at this step" (Ada.isZero contractorPayoutAmount) &&
                                traceIfFalse "TotalDeposited must be zero to start a new project" (totalDeposited dat == 0) &&
                                traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem StartProject) &&
                                traceIfFalse "Contractor can only StartProject if confirmedByInspector" (confirmedByInspector (permitName param)) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                (totalDeposited dat) == (totalDeposited outDatum) &&
                                                                (totalWithdrawnContractor dat) == (totalWithdrawnContractor outDatum) && 
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum) &&
                                                                (lastBalance dat) == (lastBalance outDatum)   
                                                                )

                                    
                            Contractor (WithdrawPermitPayment) ->
                                traceIfFalse "This is a withdrawal. adaToScript must go down!" isWithdrawal &&
                                traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawPermitPayment) && 
                                traceIfFalse "Contractor can only WithdrawPermitPayment if confirmedByInspector" (confirmedByInspector (permitName param)) &&
                                traceIfFalse "Contractor can only WithdrawPermitPayment if previous action was StartProject" (previousContractorAction == StartProject) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                (totalDeposited dat) == (totalDeposited outDatum) &&
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum)
                                                                )


                            Contractor (WithdrawRoughPayment) ->
                                traceIfFalse "This is a withdrawal. adaToScript must go down!" isWithdrawal &&
                                traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawRoughPayment) && 
                                traceIfFalse "Contractor can only WithdrawRoughPayment if confirmedByInspector" (confirmedByInspector (roughName param)) &&
                                traceIfFalse "Contractor can only WithdrawRoughPayment if previous action was WithdrawPermitPayment" (previousContractorAction == WithdrawPermitPayment) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                (totalDeposited dat) == (totalDeposited outDatum) &&
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum)
                                                                )    

                            Contractor (WithdrawDrywallPayment) ->
                                traceIfFalse "This is a withdrawal. adaToScript must go down!" isWithdrawal &&
                                traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawDrywallPayment) && 
                                traceIfFalse "Contractor can only WithdrawDrywallPayment if confirmedByInspector" (confirmedByInspector (drywallName param)) &&
                                traceIfFalse "Contractor can only WithdrawDrywallPayment if previous action was WithdrawRoughPayment" (previousContractorAction == WithdrawRoughPayment) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                (totalDeposited dat) == (totalDeposited outDatum) &&
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum)
                                                                )

                            Contractor (WithdrawFinalPayment) -> 
                                traceIfFalse "This is a withdrawal. adaToScript must go down!" isWithdrawal &&
                                traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawFinalPayment) && 
                                traceIfFalse "Contractor can only WithdrawFinalPayment if confirmedByInspector" (confirmedByInspector (finalName param)) &&
                                traceIfFalse "Contractor can only WithdrawFinalPayment if previous action was WithdrawDrywallPayment" (previousContractorAction == WithdrawDrywallPayment) &&
                                traceIfFalse "Cannot modify any other part of Datum" (
                                                                (totalDeposited dat) == (totalDeposited outDatum) &&
                                                                (totalWithdrawnHomeowner dat) == (totalWithdrawnHomeowner outDatum)
                                                                )
            