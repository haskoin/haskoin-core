{-|
Module      : Haskoin.Test.Message
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : jprupp@protonmail.ch
Stability   : experimental
Portability : POSIX
-}
module Haskoin.Util.Arbitrary.Message where

import           Haskoin.Constants
import           Haskoin.Network.Message
import           Haskoin.Util.Arbitrary.Block
import           Haskoin.Util.Arbitrary.Crypto
import           Haskoin.Util.Arbitrary.Network
import           Haskoin.Util.Arbitrary.Transaction
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
