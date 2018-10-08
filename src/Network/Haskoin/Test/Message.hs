{-|
Module      : Network.Haskoin.Test.Message
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.Haskoin.Test.Message where

import           Network.Haskoin.Constants
import           Network.Haskoin.Network.Message
import           Network.Haskoin.Test.Block
import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Test.Network
import           Network.Haskoin.Test.Transaction
import           Test.QuickCheck

-- | Arbitrary 'MessageHeader'.
arbitraryMessageHeader :: Gen MessageHeader
arbitraryMessageHeader =
    MessageHeader <$> arbitrary
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
