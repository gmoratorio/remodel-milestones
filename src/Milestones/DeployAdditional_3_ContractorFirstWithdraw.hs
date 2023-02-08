{-# LANGUAGE NumericUnderscores #-}

module Milestones.DeployAdditional_3_ContractorFirstWithdraw where

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Aeson                         as DataAeson

import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import           Cardano.Api
import qualified PlutusTx

import qualified Milestones.OnChain as OnChain

-- NOTE: This file was used to create the walkthrough in the README. Do not change any of these values unless you plan on creating a new workflow

-- This file DeployAdditional_3_ContractorFirstWithdraw will be withdrawing 500ADA (500_000_000 lovelace) to the contractor
-- Note: for all datum values below, the OnChain code will enforce that totals line up

-- Set desired lastContractorAction to new value for a contractor action
lastContractorAction :: OnChain.RedeemContractor
lastContractorAction = OnChain.WithdrawPermitPayment

-- this value should remain the same for a contractor withdrawal
totalDeposited :: Integer
totalDeposited = 500_000_000

-- this value should reflect the correct balance after contractor withdrawal
lastBalance :: Integer
lastBalance = 2_000_000

-- this value should increase by withdrawn amount for a contractor withdraw action
totalWithdrawnContractor :: Integer
totalWithdrawnContractor = 500_000_000

-- this value should remain the same since homeowner is not withdrawing
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
    in writeJSON "src/Milestones/Deploy/3-deploy-contractor-first-withdraw-datum.json" d