{-|
  Arbitrary types for Network.Haskoin.Node.Message
-}
module Network.Haskoin.Test.Message where

import           Network.Haskoin.Network.Message
import           Network.Haskoin.Test.Block
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Network
import           Network.Haskoin.Test.Transaction
import           Test.QuickCheck

-- | Arbitrary MessageHeader
arbitraryMessageHeader :: Gen MessageHeader
arbitraryMessageHeader =
    MessageHeader <$> arbitrary
                  <*> arbitraryMessageCommand
                  <*> arbitrary
                  <*> arbitraryCheckSum32

-- | Arbitrary Message
arbitraryMessage :: Gen Message
arbitraryMessage =
    oneof
        [ MVersion <$> arbitraryVersion
        , return MVerAck
        , MAddr <$> arbitraryAddr1
        , MInv <$> arbitraryInv1
        , MGetData <$> arbitraryGetData
        , MNotFound <$> arbitraryNotFound
        , MGetBlocks <$> arbitraryGetBlocks
        , MGetHeaders <$> arbitraryGetHeaders
        , MTx <$> arbitraryTx
        , MBlock <$> arbitraryBlock
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
