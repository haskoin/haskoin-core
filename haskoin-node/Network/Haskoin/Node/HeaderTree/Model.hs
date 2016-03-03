{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.Haskoin.Node.HeaderTree.Model where

import           Data.Word                             (Word32)
import           Database.Persist.TH                   (mkMigrate, mkPersist,
                                                        persistLowerCase, share,
                                                        sqlSettings)
import           Network.Haskoin.Node.HeaderTree.Types

share [mkPersist sqlSettings, mkMigrate "migrateHeaderTree"] [persistLowerCase|
NodeBlock
    hash         NodeHash      maxlen=64
    version      Word32
    prev         NodeHash      maxlen=64
    merkleRoot   MerkleHash    maxlen=64
    time         Timestamp
    bits         Word32
    nonce        Word32
    height       BlockHeight
    work         Work          maxlen=64
    medianTimes  [Timestamp]
    minWork      Word32
    chain        Int
    pivots       [BlockHeight]
    pivotChains  [Int]
    UniqueHash   hash
    UniqueChain  chain height
    deriving     Show
    deriving     Eq
|]
