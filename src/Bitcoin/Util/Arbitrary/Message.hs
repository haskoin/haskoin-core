-- |
-- Stability   : experimental
-- Portability : POSIX
module Bitcoin.Util.Arbitrary.Message where

import Bitcoin.Data
import Bitcoin.Network.Message
import Bitcoin.Util.Arbitrary.Block
import Bitcoin.Util.Arbitrary.Crypto
import Bitcoin.Util.Arbitrary.Network
import Bitcoin.Util.Arbitrary.Transaction
import Test.QuickCheck


-- | Arbitrary 'MessageHeader'.
arbitraryMessageHeader :: Gen MessageHeader
arbitraryMessageHeader =
    MessageHeader
        <$> arbitrary
        <*> arbitraryMessageCommand
        <*> arbitrary
        <*> arbitraryCheckSum32


-- | Arbitrary 'Message'.
arbitraryMessage :: Network -> Gen Message
arbitraryMessage net =
    oneof
        [ MVersion <$> arbitraryVersion
        , return MVerAck
        , MAddr <$> arbitraryAddr1
        , MInv <$> arbitraryInv1
        , MGetData <$> arbitraryGetData
        , MNotFound <$> arbitraryNotFound
        , MGetBlocks <$> arbitraryGetBlocks
        , MGetHeaders <$> arbitraryGetHeaders
        , MTx <$> arbitraryTx net
        , MBlock <$> arbitraryBlock net
        , MMerkleBlock <$> arbitraryMerkleBlock
        , MHeaders <$> arbitraryHeaders
        , return MGetAddr
        , MFilterLoad <$> arbitraryFilterLoad
        , MFilterAdd <$> arbitraryFilterAdd
        , return MFilterClear
        , MPing <$> arbitraryPing
        , MPong <$> arbitraryPong
        , MAlert <$> arbitraryAlert
        , MReject <$> arbitraryReject
        , return MSendHeaders
        ]
