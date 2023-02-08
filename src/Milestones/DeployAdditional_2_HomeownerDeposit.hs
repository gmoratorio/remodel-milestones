{-# LANGUAGE NumericUnderscores #-}

module Milestones.DeployAdditional_2_HomeownerDeposit where

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Aeson                         as DataAeson

import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import           Cardano.Api
import qualified PlutusTx

import qualified Milestones.OnChain as OnChain

-- NOTE: This file was used to create the walkthrough in the README. Do not change any of these values unless you plan on creating a new workflow

-- This file DeployAdditional_2_HomeownerDeposit will be depositing 500ADA (500_000_000 lovelace) + the 2ADA MinUTXO already there
-- Note: for all datum values below, the OnChain code will enforce that totals line up

-- Set desired lastContractorAction to new value for a contractor action, 
-- or leave same as previous value for a homeowner action
lastContractorAction :: OnChain.RedeemContractor
lastContractorAction = OnChain.StartProject

-- this value should increase by deposited amount for a homeowner deposit action
totalDeposited :: Integer
totalDeposited = 500_000_000

-- this value should reflect the correct balance after either a homeowner deposit or a homeowner/contractor withdrawal
lastBalance :: Integer
lastBalance = 502_000_000

-- this value should increase by withdrawn amount for a contractor withdraw action
totalWithdrawnContractor :: Integer
totalWithdrawnContractor = 0

-- this value should increase by withdrawn amount for a homeowner withdraw action
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
    in writeJSON "src/Milestones/Deploy/2-deploy-homeowner-deposit-datum.json" d