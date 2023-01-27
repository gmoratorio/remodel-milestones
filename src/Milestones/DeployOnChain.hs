module Milestones.DeployOnChain where

import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Base16             as B16
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Short              as SBS
import qualified Data.String                        as DataString (IsString(fromString))
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

-- must be in lovelaces
totalCost :: Integer
totalCost = 10000000000 -- ENTER YOUR TOTAL COST HERE. 10k ADA default here

-- policyID for this project, created by contractor using Milestones.DeployNFT.hs
-- should be at Milestones/Deploy/policyID after running mintMilestonesNFT.sh script
projectPolicyId :: PlutusPrelude.BuiltinByteString
projectPolicyId = "MILESTONE_PROJECT_POLICY_ID_HERE"

-- policyID for reference InspectorNFTs, created by inspector using Inspector.Deploy.hs
-- should be at Inspector/Deploy/policyID after running any inspector script that calls mintInspectorNFT.sh
inspectionPolicyId :: PlutusPrelude.BuiltinByteString
inspectionPolicyId = "INSPECTOR_REFERENCE_NFT_POLICY_ID_HERE"

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
lastContractorAction = OnChain.StartProject

-- all of these are enforced by OnChain to start at 0
totalDeposited :: Integer
totalDeposited = 0

lastBalance :: Integer
lastBalance = 0

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
    in writeJSON "src/Milestones/Deploy/parameterized-initial-datum.json" d

writeParameterized :: IO (Either (FileError ()) ())
writeParameterized = writeValidator "src/Milestones/Deploy/Milestones.plutus" $ OnChain.validator parameters

