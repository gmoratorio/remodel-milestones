module Milestones.DeployNFT where

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


import qualified Milestones.Mint as Mint

-- ByteString (string) of the pkh of the contractor's minting wallet
contractor :: B.ByteString
contractor = "ENTER_CONTRACTOR_PKH_HERE"

-- ByteString (string) of the token name for the Milestone's thread NFT
tokenName :: B.ByteString
tokenName = "ENTER_TOKEN_NAME_HERE"

-- Deadline (Integer in POSIXTime) after which the NFT contract will lock
-- https://www.epochconverter.com/
deadline :: LedgerApiV2.POSIXTime
deadline = 1674595130000 -- Enter updated deadline here

-- available utxo (with hash) of the minter wallet above
-- this utxo will be used to make the script unique,
-- and will be consumed by mintMilestonesNFT.sh to create a thread NFT
utxo :: String
utxo = "ENTER_UTXO_HERE"

parameters :: Mint.MilestoneMintParam
parameters = Mint.MilestoneMintParam 
        { Mint.utxo         = convertToUtxo utxo
        , Mint.tokenName    = convertToTokenName tokenName
        , Mint.contractor   = convertToPubKeyHash contractor
        , Mint.deadline     = deadline
        }
           

convertToUtxo :: String -> LedgerApiV2.TxOutRef
convertToUtxo s = 
    let (x, _ : y) = span (/= '#') s
    in
        LedgerApiV2.TxOutRef
            {
                LedgerApiV2.txOutRefId = DataString.fromString x,
                LedgerApiV2.txOutRefIdx = read y
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
    writeRedeemer
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
writeDatumUnit = writeJSON "src/Milestones/Deploy/unit.json" ()

writeRedeemer :: IO ()
writeRedeemer = 
    let mintPermit = ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData ()
    in
        writeJSON "src/Milestones/Deploy/redeemer-mint.json" mintPermit

writeMint :: IO (Either (FileError ()) ())
writeMint = writeValidator "src/Milestones/Deploy/Mint.plutus" $ Mint.policy parameters