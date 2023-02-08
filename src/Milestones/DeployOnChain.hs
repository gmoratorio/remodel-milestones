{-# LANGUAGE NumericUnderscores #-}

module Milestones.DeployOnChain where

import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Base16             as B16
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Short              as SBS
import qualified Data.Aeson                         as DataAeson
import           Codec.Serialise (serialise)

import qualified Ledger                             as Ledger
import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import qualified PlutusTx.Prelude                   as PlutusPrelude
import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts           as ScriptsLedger

import qualified Milestones.OnChain as OnChain

-- ByteString (string) of the pkh of the contractor's wallet
contractor :: B.ByteString
contractor = "ENTER_CONTRACTOR_PKH_HERE"

-- ByteString (string) of the pkh of the homeowner's wallet
homeowner :: B.ByteString
homeowner = "ENTER_HOMEOWNER_PKH_HERE"

-- policyID for reference InspectorNFTs, created by inspector using Inspector.Deploy.hs
-- should be at Inspector/Deploy/policyID after running any inspector script that calls mintInspectorNFT.sh

inspectionPolicyId :: B.ByteString
inspectionPolicyId = "ENTER_POLICY_ID_HERE"

-- policyID for this project, created by contractor using Milestones.DeployNFT.hs
-- should be at Milestones/Deploy/policyID after running mintMilestonesNFT.sh script
projectPolicyId :: B.ByteString
projectPolicyId = "ENTER_POLICY_ID_HERE"


-- Values below can be changed, but their corresponding values in scripts and datums will need to be updated as well
-- To use provided walkthrough, leave these values below as-is
-- must be in lovelaces
totalCost :: Integer
totalCost = 2_000_000_000 -- ENTER YOUR TOTAL COST HERE. 2k ADA default here

-- payments below MUST add up to totalCost or validator will fail
-- must be in lovelaces
deposit :: Integer
deposit = 500_000_000 -- ENTER AMOUNT HERE

-- must be in lovelaces
secondPayment :: Integer
secondPayment = 500_000_000 -- ENTER AMOUNT HERE

-- must be in lovelaces
thirdPayment :: Integer
thirdPayment = 500_000_000 -- ENTER AMOUNT HERE

-- must be in lovelaces
finalPayment :: Integer
finalPayment = 500_000_000 -- ENTER AMOUNT HERE


-- these match the InspectorNFT Names in Inspector/Deploy.hs - DO NOT CHANGE
permitName :: B.ByteString
permitName = "PermitIssued"

roughName :: B.ByteString
roughName = "RoughPassed"

drywallName :: B.ByteString
drywallName = "DrywallPassed"

finalName :: B.ByteString
finalName = "FinalPassed"

closedName :: B.ByteString
closedName = "ClosedIncomplete"


parameters :: OnChain.MilestoneParam
parameters = OnChain.MilestoneParam
                { OnChain.homeowner             = convertToPubKeyHash homeowner
                , OnChain.contractor            = convertToPubKeyHash contractor
                , OnChain.totalCost             = totalCost
                , OnChain.deposit               = deposit
                , OnChain.secondPayment         = secondPayment
                , OnChain.thirdPayment          = thirdPayment
                , OnChain.finalPayment          = finalPayment
                , OnChain.projectPolicyId       = decodeHex projectPolicyId
                , OnChain.inspectionPolicyId    = decodeHex inspectionPolicyId
                , OnChain.permitName            = convertToTokenName permitName
                , OnChain.roughName             = convertToTokenName roughName
                , OnChain.drywallName           = convertToTokenName drywallName
                , OnChain.finalName             = convertToTokenName finalName
                , OnChain.closedName            = convertToTokenName closedName
                }

convertToTokenName :: B.ByteString -> LedgerApiV2.TokenName
convertToTokenName b = LedgerApiV2.TokenName (PlutusPrelude.toBuiltin b)

decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBs = 
    case getTx of
        Right decHex -> PlutusPrelude.toBuiltin(decHex)
        Left _ -> PlutusPrelude.emptyByteString
    where 
        getTx :: Either String B.ByteString
        getTx = B16.decode hexBs

convertToPubKeyHash :: B.ByteString -> Ledger.PaymentPubKeyHash
convertToPubKeyHash b = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)


-- Datum must be initialized to StartProject state
lastContractorAction :: OnChain.RedeemContractor
lastContractorAction = OnChain.NotStarted

-- this is enforced by OnChain to start at 2ADA for MinUTXO that comes with the AuthNFT
lastBalance :: Integer
lastBalance = 2_000_000

-- these are enforced by OnChain to start at 0
totalDeposited :: Integer
totalDeposited = 0

totalWithdrawnContractor :: Integer
totalWithdrawnContractor = 0

totalWithdrawnHomeowner :: Integer
totalWithdrawnHomeowner = 0

milestoneDatum :: OnChain.MilestoneDatum
milestoneDatum = OnChain.MilestoneDatum 
                        { OnChain.lastContractorAction = lastContractorAction
                        , OnChain.totalDeposited = totalDeposited
                        , OnChain.lastBalance = lastBalance
                        , OnChain.totalWithdrawnContractor = totalWithdrawnContractor
                        , OnChain.totalWithdrawnHomeowner = totalWithdrawnHomeowner
                        }

main :: IO()
main = do
    writeDatumUnit
    writeMilestoneDatum
    writeRedeemerHomeownerAddFunds
    writeRedeemerHomeownerWithdrawFunds
    writeRedeemerContractorStartProject
    writeRedeemerContractorPermitPayment
    writeRedeemerContractorRoughPayment
    writeRedeemerContractorDrywallPayment
    writeRedeemerContractorFinalPayment
    _ <- writeParameterized



    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs)  = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)       = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)      = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)          = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)         = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeDatumUnit :: IO ()
writeDatumUnit = writeJSON "src/Milestones/Deploy/unit.json" ()

writeMilestoneDatum :: IO ()
writeMilestoneDatum = 
    let d = PlutusTx.toBuiltinData milestoneDatum
    in writeJSON "src/Milestones/Deploy/0-parameterized-initial-datum.json" d

writeRedeemerHomeownerAddFunds :: IO ()
writeRedeemerHomeownerAddFunds = 
    let ho = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Homeowner (OnChain.HomeownerAddFunds)
    in
        writeJSON "src/Milestones/Deploy/redeemer-homeowner-add-funds.json" ho

writeRedeemerHomeownerWithdrawFunds :: IO ()
writeRedeemerHomeownerWithdrawFunds = 
    let ho = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Homeowner (OnChain.HomeownerWithdrawFunds)
    in
        writeJSON "src/Milestones/Deploy/redeemer-homeowner-withdraw-funds.json" ho

writeRedeemerContractorNotStarted :: IO ()
writeRedeemerContractorNotStarted = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.NotStarted)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-not-started.json" ctr

writeRedeemerContractorStartProject :: IO ()
writeRedeemerContractorStartProject = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.StartProject)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-start-project.json" ctr

writeRedeemerContractorPermitPayment :: IO ()
writeRedeemerContractorPermitPayment = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.WithdrawPermitPayment)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-permit-payment.json" ctr

writeRedeemerContractorRoughPayment :: IO ()
writeRedeemerContractorRoughPayment = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.WithdrawRoughPayment)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-rough-payment.json" ctr

writeRedeemerContractorDrywallPayment :: IO ()
writeRedeemerContractorDrywallPayment = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.WithdrawDrywallPayment)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-drywall-payment.json" ctr

writeRedeemerContractorFinalPayment :: IO ()
writeRedeemerContractorFinalPayment = 
    let ctr = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Contractor (OnChain.WithdrawFinalPayment)
    in
        writeJSON "src/Milestones/Deploy/redeemer-contractor-final-payment.json" ctr

writeParameterized :: IO (Either (FileError ()) ())
writeParameterized = writeValidator "src/Milestones/Deploy/Milestones.plutus" $ OnChain.validator parameters

