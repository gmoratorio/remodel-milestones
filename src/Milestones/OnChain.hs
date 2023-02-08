{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}


module Milestones.OnChain where

import PlutusTx.Prelude                   
import qualified Ledger                                             as Ledger
import qualified Ledger.Ada                                         as Ada
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Contexts                          as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value                             as LedgerValueV1
import qualified Plutus.V2.Ledger.Api                               as LedgerApiV2
import qualified Plutus.V2.Ledger.Tx                                as PlutusV2LedgerTx
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators    as V2UtilsTypeScripts
import qualified Plutus.Script.Utils.V2.Scripts                     as V2UtilsScripts
import qualified Plutus.Script.Utils.Typed                          as UtilsTypeScripts
import qualified Ledger.Typed.Scripts                               as Scripts


data MilestoneParam = MilestoneParam 
                { homeowner ::          Ledger.PaymentPubKeyHash
                , contractor ::         Ledger.PaymentPubKeyHash
                , totalCost ::          Integer
                , deposit ::            Integer
                , secondPayment ::      Integer
                , thirdPayment ::       Integer
                , finalPayment ::       Integer
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

data RedeemHomeowner =  HomeownerAddFunds | HomeownerWithdrawFunds

instance Eq RedeemHomeowner where
    HomeownerAddFunds == HomeownerAddFunds              = True
    HomeownerWithdrawFunds == HomeownerWithdrawFunds    = True
    _ == _                                              = False

PlutusTx.unstableMakeIsData ''RedeemHomeowner
PlutusTx.makeLift ''RedeemHomeowner

data RedeemContractor = NotStarted |
                        StartProject |
                        WithdrawPermitPayment | 
                        WithdrawRoughPayment | 
                        WithdrawDrywallPayment | 
                        WithdrawFinalPayment

instance Eq RedeemContractor where
    NotStarted == NotStarted                            = True
    StartProject == StartProject                        = True
    WithdrawPermitPayment == WithdrawPermitPayment      = True
    WithdrawRoughPayment == WithdrawRoughPayment        = True
    WithdrawDrywallPayment == WithdrawDrywallPayment    = True
    WithdrawFinalPayment == WithdrawFinalPayment        = True
    _ == _                                              = False


PlutusTx.unstableMakeIsData ''RedeemContractor
PlutusTx.makeLift ''RedeemContractor

data MilestoneRedeem = Homeowner RedeemHomeowner | Contractor RedeemContractor
instance Eq MilestoneRedeem where
    Homeowner (HomeownerAddFunds) == Homeowner (HomeownerAddFunds)              = True
    Homeowner (HomeownerWithdrawFunds) == Homeowner (HomeownerWithdrawFunds)    = True
    Contractor (StartProject) == Contractor (StartProject)                      = True
    Contractor (WithdrawPermitPayment) == Contractor (WithdrawPermitPayment)    = True
    Contractor (WithdrawRoughPayment) == Contractor (WithdrawRoughPayment)      = True
    Contractor (WithdrawDrywallPayment) == Contractor (WithdrawDrywallPayment)  = True
    Contractor (WithdrawFinalPayment) == Contractor (WithdrawFinalPayment)      = True
    _ == _                                                                      = False

PlutusTx.unstableMakeIsData ''MilestoneRedeem
PlutusTx.makeLift ''MilestoneRedeem

data MilestoneDatum = MilestoneDatum
                { lastContractorAction :: RedeemContractor
                , totalDeposited :: Integer
                , lastBalance :: Integer
                , totalWithdrawnContractor :: Integer
                , totalWithdrawnHomeowner :: Integer
                }

PlutusTx.unstableMakeIsData ''MilestoneDatum
PlutusTx.makeLift ''MilestoneDatum

data Milestone 
instance V2UtilsTypeScripts.ValidatorTypes Milestone where
    type instance RedeemerType Milestone    = MilestoneRedeem
    type instance DatumType Milestone       = MilestoneDatum

{-# INLINABLE milestoneValidator #-}
milestoneValidator :: MilestoneParam -> MilestoneDatum -> MilestoneRedeem -> LedgerContextsV2.ScriptContext -> Bool
milestoneValidator MilestoneParam{..} MilestoneDatum{..} redeem sc =
    
    traceIfFalse "Sum of contractor payouts does not match totalCost. New contract needed." (
    (deposit + secondPayment + thirdPayment + finalPayment) == totalCost) &&

    let
        txinfo :: LedgerContextsV2.TxInfo
        txinfo = LedgerContextsV2.scriptContextTxInfo sc

        -- inputs we're consuming
        inputs :: [LedgerContextsV2.TxInInfo]
        inputs = LedgerContextsV2.txInfoInputs txinfo

        -- inputs we're only referencing
        referenceInputs :: [LedgerContextsV2.TxInInfo]
        referenceInputs = LedgerContextsV2.txInfoReferenceInputs txinfo

        -- all outputs being produced
        outputs :: [LedgerContextsV2.TxOut]
        outputs = LedgerContextsV2.txInfoOutputs txinfo

        -- outputs going back to the script they're spent from
        continuingOutputs :: [LedgerContextsV2.TxOut]
        continuingOutputs = LedgerContextsV2.getContinuingOutputs sc

        signatures :: [LedgerApiV2.PubKeyHash]
        signatures = LedgerContextsV2.txInfoSignatories txinfo

        authPolicyId :: LedgerApiV2.CurrencySymbol
        authPolicyId =  LedgerApiV2.CurrencySymbol projectPolicyId

        inspectorPolicyId :: LedgerApiV2.CurrencySymbol
        inspectorPolicyId =  LedgerApiV2.CurrencySymbol inspectionPolicyId

        getInputValue :: LedgerContextsV2.TxInInfo -> LedgerValueV1.Value
        getInputValue = LedgerContextsV2.txOutValue . LedgerContextsV2.txInInfoResolved

        valueContainsAuthPolicy :: LedgerValueV1.Value -> Bool
        valueContainsAuthPolicy value =
                let currencies = LedgerValueV1.flattenValue $ value
                in any(\(policyId, _, _) -> policyId == authPolicyId) currencies

        valueContainsInspectorPolicy :: LedgerValueV1.Value -> Bool
        valueContainsInspectorPolicy value =
                let currencies = LedgerValueV1.flattenValue $ value
                in any(\(policyId, _, _) -> policyId == inspectorPolicyId) currencies

        authNFTBackToScript :: Bool
        authNFTBackToScript = isJust $ find (\txout -> valueContainsAuthPolicy $ LedgerContextsV2.txOutValue txout) continuingOutputs

        inputHasAuthNFT :: Bool
        inputHasAuthNFT = 
            let maybeAuthTx = LedgerContextsV2.findOwnInput sc
            in 
                case maybeAuthTx of
                    Just (txInInfo) -> valueContainsAuthPolicy $ getInputValue txInInfo
                    _ -> False

        referenceInputIsInspectorNFT :: Bool
        referenceInputIsInspectorNFT = 
            let [onlyInput] = referenceInputs
            in valueContainsInspectorPolicy $ getInputValue onlyInput

        exactlyTwoInputs :: Bool
        exactlyTwoInputs = length inputs == 2

        exactlyOneSignature :: Bool
        exactlyOneSignature = length signatures == 1

        exactlyOneReferenceInput :: Bool
        exactlyOneReferenceInput = length referenceInputs == 1

        exactlyOneContinuingOutput :: Bool
        exactlyOneContinuingOutput = length continuingOutputs == 1
    in
        traceIfFalse "Each transaction should have exactly two inputs: the authNFT and one signer UTxO" exactlyTwoInputs &&
        traceIfFalse "Each transaction should only have one signature, eithere from the Contractor or Homeowner" exactlyOneSignature &&
        traceIfFalse "All transactions must include authNFT for authentication" inputHasAuthNFT &&
        traceIfFalse "All transactions must a reference to an Inspector NFT" referenceInputIsInspectorNFT &&
        traceIfFalse "All transactions must send authNFT back to script" authNFTBackToScript &&
        traceIfFalse "Only one reference input can be provided at a time" exactlyOneReferenceInput &&
        traceIfFalse "Only one UTxO can be sent back to the script at a time" exactlyOneContinuingOutput && 

    let
        -- at this point we're sure that we only have 1 output back to the script
        outputToScript :: LedgerContextsV2.TxOut
        outputToScript = head continuingOutputs

        continuingOutValue :: LedgerValueV1.Value
        continuingOutValue = LedgerContextsV2.txOutValue outputToScript

        amountOfAdaToScript :: Integer
        amountOfAdaToScript = LedgerValueV1.valueOf continuingOutValue LedgerApiV2.adaSymbol LedgerApiV2.adaToken


        MilestoneDatum  { lastContractorAction      = nextContractorAction
                        , totalDeposited            = nextTotalDeposited
                        , lastBalance               = nextLastBalance
                        , totalWithdrawnContractor  = nextTotalWithdrawnContractor
                        , totalWithdrawnHomeowner   = nextTotalWithdrawnHomeowner 
                        } = case PlutusV2LedgerTx.txOutDatum outputToScript of
                                    PlutusV2LedgerTx.OutputDatum (LedgerApiV2.Datum d) -> LedgerApiV2.unsafeFromBuiltinData d
                                    PlutusV2LedgerTx.OutputDatumHash dh -> 
                                        case LedgerContextsV2.findDatum dh txinfo of
                                            Just (LedgerApiV2.Datum d) -> LedgerApiV2.unsafeFromBuiltinData d
                                            Nothing -> traceError "Could not find Datum from DatumHash"

                                    PlutusV2LedgerTx.NoOutputDatum -> traceError "No datum in outputToScript"

        isWithdrawal :: Bool
        isWithdrawal = amountOfAdaToScript < lastBalance

        isDeposit :: Bool
        isDeposit = amountOfAdaToScript > lastBalance

        datumActionMatchesRedeem :: RedeemContractor -> Bool
        datumActionMatchesRedeem = (nextContractorAction ==)

        -- a this point we're sure we have the right policyID, so we just need to check for specific tokenNames
        confirmedByInspector :: LedgerApiV2.TokenName -> Bool
        confirmedByInspector tn = 
            case referenceInputs of
                [onlyInput] ->
                    let flattenedInspectorValue = filter (\(curSymbol, _, _) -> curSymbol == inspectorPolicyId) $ LedgerValueV1.flattenValue $ getInputValue onlyInput
                    in 
                        case flattenedInspectorValue of
                            [(_, inspectorTokenName, _)]    -> tn == inspectorTokenName
                            _                               -> False
                _                                           -> False

        intToAda :: Integer -> Ada.Ada
        intToAda = Ada.lovelaceOf

        scheduledPayout :: RedeemContractor -> Ada.Ada
        scheduledPayout lastAction = 
            case lastAction of
                WithdrawPermitPayment   -> intToAda $ deposit
                WithdrawRoughPayment    -> intToAda $ secondPayment
                WithdrawDrywallPayment  -> intToAda $ thirdPayment
                WithdrawFinalPayment    -> intToAda $ finalPayment
                _                       -> intToAda 0

        totalDueContractor :: Ada.Ada
        totalDueContractor = 
            case lastContractorAction of
                WithdrawPermitPayment   -> scheduledPayout WithdrawPermitPayment
                WithdrawRoughPayment    -> scheduledPayout WithdrawPermitPayment <> scheduledPayout WithdrawRoughPayment
                WithdrawDrywallPayment  -> scheduledPayout WithdrawPermitPayment <> scheduledPayout WithdrawRoughPayment <> scheduledPayout WithdrawDrywallPayment
                WithdrawFinalPayment    -> intToAda totalCost
                _                       -> intToAda 0

        allowedHomewownerWithdrawal :: Ada.Ada
        allowedHomewownerWithdrawal  = 
            let totalADADeposited           = intToAda $ totalDeposited
                totalADAStillDueContractor  = totalDueContractor - (intToAda $ totalWithdrawnContractor)
                totalADAHomeownerWithdrawn  = intToAda $ totalWithdrawnHomeowner
            in
                totalADADeposited - totalADAStillDueContractor - totalADAHomeownerWithdrawn

        -- at this point we're certain we only have one signature
        -- so when we run these additional checks we know that only either the Contractor or Homeowner signed
        signedByContractor :: Bool
        signedByContractor = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash contractor

        signedByHomeowner :: Bool
        signedByHomeowner = LedgerContextsV2.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash homeowner

        totalAdaSpentFromContract :: Ada.Ada
        totalAdaSpentFromContract = intToAda (lastBalance - amountOfAdaToScript)

        valueOnlyToStakeholderAndScript :: Ledger.PaymentPubKeyHash -> Bool
        valueOnlyToStakeholderAndScript stakeholder = 
                let stakeholderPkh = Ledger.unPaymentPubKeyHash stakeholder
                    scriptVH = LedgerContextsV2.ownHash sc
                in all  (\txOut -> 
                            let outAddress = LedgerContextsV2.txOutAddress txOut
                            in case Ledger.toPubKeyHash outAddress of
                                    Just (pkh) -> pkh == stakeholderPkh
                                    Nothing -> case Ledger.toValidatorHash outAddress of
                                                Just (vh) -> vh == scriptVH
                                                Nothing -> False
                                
                        ) outputs


    in
        traceIfFalse "nextLastBalance must match amountOfAdaToScript" (nextLastBalance == amountOfAdaToScript) &&

        case redeem of 
            Homeowner (_) ->
                traceIfFalse "Must be signed by homeowner" signedByHomeowner &&
                traceIfFalse "Funds can only go to Homeowner and Milestones address at this step" (valueOnlyToStakeholderAndScript homeowner) && 

                case redeem of
                    Homeowner (HomeownerAddFunds) ->
                        traceIfFalse "Cannot add any more funds if project is ClosedIncomplete" (not (confirmedByInspector closedName)) &&
                        traceIfFalse "Cannot add any more funds if totalCost was previously met" (totalDeposited < totalCost) &&
                        traceIfFalse "Deposit would pass project's totalCost" (nextTotalDeposited <= totalCost) &&
                        traceIfFalse "This is a deposit. totalDeposited to script must go up!" isDeposit &&
                        traceIfFalse "totalDeposited increase must match lastBalance increase" 
                                                    ((nextTotalDeposited - totalDeposited) == (nextLastBalance - lastBalance)) &&
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        lastContractorAction == nextContractorAction && 
                                                        totalWithdrawnContractor == nextTotalWithdrawnContractor && 
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner
                                                        )

                    Homeowner (HomeownerWithdrawFunds) ->
                        traceIfFalse "Homeowner cannot withdraw funds if project is successfully complete" (lastContractorAction /= WithdrawFinalPayment) &&
                        traceIfFalse "Homeowner cannot withdraw more than what they're allowed" (totalAdaSpentFromContract <= allowedHomewownerWithdrawal) &&
                        traceIfFalse "Homeowner can only withdraw funds if project is ClosedIncomplete" (confirmedByInspector closedName) &&
                        traceIfFalse "totalWithdrawnHomeowner increase must match lastBalance decrease" 
                                                    ((nextTotalWithdrawnHomeowner - totalWithdrawnHomeowner) == (lastBalance - nextLastBalance)) &&
                        traceIfFalse "This is a withdrawal. amountOfAdaToScript must go down!" isWithdrawal &&
                        traceIfFalse "Cannot modify any other part of Datum" (
                                        (lastContractorAction == nextContractorAction) && 
                                        totalWithdrawnContractor == nextTotalWithdrawnContractor && 
                                        totalDeposited == nextTotalDeposited
                                        )

            
            Contractor (_) ->
                traceIfFalse "Must be signed by contractor" signedByContractor &&
                traceIfFalse "Funds can only go to Contractor and Milestones address at this step" (valueOnlyToStakeholderAndScript contractor) && 
                traceIfFalse "totalWithdrawnContractor increase must match lastBalance decrease" 
                        ((nextTotalWithdrawnContractor - totalWithdrawnContractor) == (lastBalance - nextLastBalance)) &&

                case redeem of
                    Contractor (NotStarted) ->
                        traceError "No redemption possible of project is NotStarted"

                    Contractor (StartProject) ->
                        traceIfFalse "Contractor can only StartProject if previous action was NotStarted" (lastContractorAction == NotStarted) &&
                        traceIfFalse "Contractor can only StartProject if confirmedByInspector" (confirmedByInspector permitName) &&
                        traceIfFalse "lastBalance must be exactly the 2ADA minUTXO to start a new project" (lastBalance == 2_000_000) &&
                        traceIfFalse "totalDeposited must be zero to start a new project" (totalDeposited == 0) &&
                        traceIfFalse "totalWithdrawnContractor must be zero to start a new project" (totalWithdrawnContractor == 0) &&
                        traceIfFalse "totalWithdrawnHomeowner must be zero to start a new project" (totalWithdrawnHomeowner == 0) &&
                        traceIfFalse "Contract cannot spend any funds at this step" (Ada.isZero totalAdaSpentFromContract) &&
                        traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem StartProject) &&
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        totalDeposited == nextTotalDeposited &&
                                                        totalWithdrawnContractor == nextTotalWithdrawnContractor && 
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner &&
                                                        lastBalance == nextLastBalance   
                                                        )

                    Contractor (WithdrawPermitPayment) ->
                        traceIfFalse "Contractor can only WithdrawPermitPayment if previous action was StartProject" (lastContractorAction == StartProject) &&
                        traceIfFalse "Contractor can only WithdrawPermitPayment if confirmedByInspector" (confirmedByInspector permitName) &&
                        traceIfFalse "This is a withdrawal. amountOfAdaToScript must go down!" isWithdrawal &&
                        traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawPermitPayment) && 
                        traceIfFalse "Contractor payout must match schedule for this milestone" (totalAdaSpentFromContract == (scheduledPayout WithdrawPermitPayment)) &&
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        totalDeposited == nextTotalDeposited &&
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner
                                                        ) 

                    Contractor (WithdrawRoughPayment) ->
                        traceIfFalse "Contractor can only WithdrawRoughPayment if previous action was WithdrawPermitPayment" (lastContractorAction == WithdrawPermitPayment) &&
                        traceIfFalse "Contractor can only WithdrawRoughPayment if confirmedByInspector" (confirmedByInspector roughName) &&
                        traceIfFalse "This is a withdrawal. amountOfAdaToScript must go down!" isWithdrawal &&
                        traceIfFalse "Contractor payout must match schedule for this milestone" (totalAdaSpentFromContract == scheduledPayout WithdrawRoughPayment) &&
                        traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawRoughPayment) && 
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        totalDeposited == nextTotalDeposited &&
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner
                                                        )

                    Contractor (WithdrawDrywallPayment) ->
                        traceIfFalse "Contractor can only WithdrawDrywallPayment if previous action was WithdrawRoughPayment" (lastContractorAction == WithdrawRoughPayment) &&
                        traceIfFalse "Contractor can only WithdrawDrywallPayment if confirmedByInspector" (confirmedByInspector drywallName) &&
                        traceIfFalse "This is a withdrawal. amountOfAdaToScript must go down!" isWithdrawal &&
                        traceIfFalse "Contractor payout must match schedule for this milestone" (totalAdaSpentFromContract == scheduledPayout WithdrawDrywallPayment) &&
                        traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawDrywallPayment) && 
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        totalDeposited == nextTotalDeposited &&
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner
                                                        )

                    Contractor (WithdrawFinalPayment) -> 
                        traceIfFalse "Contractor can only WithdrawFinalPayment if previous action was WithdrawDrywallPayment" (lastContractorAction == WithdrawDrywallPayment) &&
                        traceIfFalse "Contractor can only WithdrawFinalPayment if confirmedByInspector" (confirmedByInspector finalName) &&
                        traceIfFalse "This is a withdrawal. amountOfAdaToScript must go down!" isWithdrawal &&
                        traceIfFalse "Contractor payout must match schedule for this milestone" (totalAdaSpentFromContract == scheduledPayout WithdrawFinalPayment) &&
                        traceIfFalse "OutputDatum contractor action must match redeemer action" (datumActionMatchesRedeem WithdrawFinalPayment) && 
                        traceIfFalse "Cannot modify any other part of Datum" (
                                                        totalDeposited == nextTotalDeposited &&
                                                        totalWithdrawnHomeowner == nextTotalWithdrawnHomeowner
                                                        )

milestoneCompile :: MilestoneParam -> V2UtilsTypeScripts.TypedValidator Milestone 
milestoneCompile milestoneParam = V2UtilsTypeScripts.mkTypedValidator @Milestone 
    ($$(PlutusTx.compile [|| milestoneValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode milestoneParam)
    $$(PlutusTx.compile [|| wrap ||]) where 
        wrap = UtilsTypeScripts.mkUntypedValidator

validator :: MilestoneParam -> V2UtilsScripts.Validator
validator = Scripts.validatorScript . milestoneCompile

scriptHash :: MilestoneParam -> V2UtilsScripts.ValidatorHash
scriptHash = V2UtilsScripts.validatorHash . validator

scriptAddress :: MilestoneParam -> Ledger.Address
scriptAddress = Ledger.scriptHashAddress . scriptHash