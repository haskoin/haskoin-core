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
import           Network.Haskoin.Block
import           Network.Haskoin.Node.HeaderTree.Types

share [mkPersist sqlSettings, mkMigrate "migrateHeaderTree"] [persistLowerCase|
NodeBlock
    hash         ShortHash
    header       NodeHeader    maxlen=80
    work         Work
    height       BlockHeight
    chain        Word32
    UniqueHash   hash
    UniqueChain  chain height
    deriving     Show
    deriving     Eq
|]
