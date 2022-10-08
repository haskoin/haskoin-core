module Main (main) where

import Bitcoin (PartiallySignedTransaction, SecKey)
import qualified Bitcoin as H
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (pack)
import System.Environment (getArgs)


main :: IO ()
main = do
    keyText <- pack . head <$> getArgs
    let key = maybe (error "Unable to decode key") H.secKeyData $ H.fromWif H.btcRegTest keyText
    BS.interact $ S.encode . either error (onPsbt key) . S.decode


onPsbt :: SecKey -> PartiallySignedTransaction -> PartiallySignedTransaction
onPsbt key = H.signPSBT H.btcRegTest (H.secKeySigner key)
