module Main (main) where

import qualified Bitcoin.Address.Bech32Spec as Bech32
import qualified Bitcoin.AddressSpec as Address
import qualified Bitcoin.BlockSpec as Block
import qualified Bitcoin.Crypto.HashSpec as Hash
import qualified Bitcoin.Crypto.SignatureSpec as Sig
import qualified Bitcoin.Keys.ExtendedSpec as Extended
import qualified Bitcoin.Keys.MnemonicSpec as Mnemonic
import qualified Bitcoin.KeysSpec as Keys
import qualified Bitcoin.NetworkSpec as Net
import qualified Bitcoin.ScriptSpec as Script
import qualified Bitcoin.Transaction.PartialSpec as Partial
import qualified Bitcoin.Transaction.TaprootSpec as Taproot
import qualified Bitcoin.TransactionSpec as Transaction
import qualified Bitcoin.UtilSpec as Utils
import Test.Hspec (hspec)


main :: IO ()
main = hspec $ do
    Address.spec
    Bech32.spec
    Block.spec
    Hash.spec
    Sig.spec
    Keys.spec
    Extended.spec
    Mnemonic.spec
    Net.spec
    Script.spec
    Transaction.spec
    Partial.spec
    Taproot.spec
    Utils.spec
    pure ()
