{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Inspector.Deploy where

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


import qualified Inspector.Mint as Mint

-- ByteString (string) of the pkh of the inspector's minting wallet
inspector :: B.ByteString
inspector = "ENTER_INSPECTOR_PKH_HERE"

-- available utxo (with hash) of the minter wallet above
-- this utxo will be used to make the script unique,
-- and will be consumed by mintPermitFromScript.sh to create PermitIssued NFT
utxo :: String
utxo = "ENTER_UTXO#INDEX_HERE"

-- NOTE: Token names below should not be changed

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

parameters :: Mint.PermitParam
parameters = Mint.PermitParam 
        { Mint.utxo = convertToUtxo utxo
        , Mint.inspector = convertToPubKeyHash inspector
        , Mint.permitName = convertToTokenName permitName
        , Mint.roughName = convertToTokenName roughName
        , Mint.drywallName = convertToTokenName drywallName
        , Mint.finalName = convertToTokenName finalName
        , Mint.closedName = convertToTokenName closedName
        }
           

convertToUtxo :: String -> LedgerApiV2.TxOutRef
convertToUtxo s = 

    case span (/= '#') s of 
        (x, _ : y) -> 
            LedgerApiV2.TxOutRef
            {
                LedgerApiV2.txOutRefId = DataString.fromString x,
                LedgerApiV2.txOutRefIdx = read y
            }
        -- this case should never happen. Only added to quiet non-exhaustive patterns error
        (_,_) ->
            LedgerApiV2.TxOutRef
            {
                LedgerApiV2.txOutRefId = DataString.fromString "",
                LedgerApiV2.txOutRefIdx = 0
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

main :: IO ()
main = do 
    writeDatumUnit
    writeRedeemerPermit
    writeRedeemerRough
    writeRedeemerDrywall
    writeRedeemerFinal
    writeRedeemerClosed
    _ <- writeMint

    return ()


dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs)  = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)       = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)      = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)          = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)         = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeDatumUnit :: IO ()
writeDatumUnit = writeJSON "src/Inspector/Deploy/unit.json" ()

writeRedeemerPermit :: IO ()
writeRedeemerPermit = 
    let mintPermit = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData Mint.PermitIssued
    in
        writeJSON "src/Inspector/Deploy/redeemer-mint-permit.json" mintPermit

writeRedeemerRough :: IO ()
writeRedeemerRough = 
    let mintRough = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData Mint.RoughPassed
    in
        writeJSON "src/Inspector/Deploy/redeemer-mint-rough.json" mintRough

writeRedeemerDrywall :: IO ()
writeRedeemerDrywall = 
    let mintDrywall = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData Mint.DrywallPassed
    in
        writeJSON "src/Inspector/Deploy/redeemer-mint-drywall.json" mintDrywall


writeRedeemerFinal :: IO ()
writeRedeemerFinal = 
    let mintFinal = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData Mint.FinalPassed
    in
        writeJSON "src/Inspector/Deploy/redeemer-mint-final.json" mintFinal

writeRedeemerClosed :: IO ()
writeRedeemerClosed = 
    let mintClosed = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData Mint.ClosedIncomplete
    in
        writeJSON "src/Inspector/Deploy/redeemer-mint-closed.json" mintClosed

writeMint :: IO (Either (FileError ()) ())
writeMint = writeValidator "src/Inspector/Deploy/Mint.plutus" $ Mint.policy parameters