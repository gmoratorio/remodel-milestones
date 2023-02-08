{-# LANGUAGE NumericUnderscores #-}

module Milestones.DeployAdditional_1_ContractorStartProject where

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Aeson                         as DataAeson

import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import           Cardano.Api
import qualified PlutusTx

import qualified Milestones.OnChain as OnChain

-- NOTE: This file was used to create the walkthrough in the README. Do not change any of these values unless you plan on creating a new workflow

-- This file DeployAdditional_1_ContractorStartProject will be starting the project, and not changing any other values
-- Note: for all datum values below, the OnChain code will enforce that totals line up

-- Contractor is setting the project to StartProject
lastContractorAction :: OnChain.RedeemContractor
lastContractorAction = OnChain.StartProject

-- This should remain the same as the initial datum
totalDeposited :: Integer
totalDeposited = 0

-- This should remain the same as the initial datum
lastBalance :: Integer
lastBalance = 2_000_000

-- This should remain the same as the initial datum
totalWithdrawnContractor :: Integer
totalWithdrawnContractor = 0

-- This should remain the same as the initial datum
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
    writeMilestoneDatum

    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs)  = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)       = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)      = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)          = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)         = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeMilestoneDatum :: IO ()
writeMilestoneDatum = 
    let d = PlutusTx.toBuiltinData milestoneDatum
    in writeJSON "src/Milestones/Deploy/1-deploy-contractor-start-project-datum.json" d