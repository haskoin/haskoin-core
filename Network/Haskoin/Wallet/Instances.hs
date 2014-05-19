{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Instances where

import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans (lift)

import qualified Data.Conduit as C (transPipe)

import Database.Persist 
    ( PersistStore
    , PersistUnique
    , PersistQuery
    , PersistMonadBackend
    , get
    , getBy
    , insert_
    , insert
    , count
    , selectKeys
    , selectFirst
    , selectSource
    , updateWhere
    , deleteBy
    , insertUnique
    , updateGet
    , replace
    , repsert
    , insertKey
    , insertMany
    , delete
    , deleteWhere
    , update
    )

instance PersistStore m => PersistStore (EitherT e m) where
    type PersistMonadBackend (EitherT e m) = PersistMonadBackend m
    get         = lift . get
    insert      = lift . insert
    insert_     = lift . insert_
    insertMany  = lift . insertMany
    insertKey k = lift . (insertKey k)
    repsert k   = lift . (repsert k)
    replace k   = lift . (replace k)
    delete      = lift . delete

instance PersistUnique m => PersistUnique (EitherT e m) where
    getBy        = lift . getBy
    deleteBy     = lift . deleteBy
    insertUnique = lift . insertUnique

instance PersistQuery m => PersistQuery (EitherT e m) where
    update k       = lift . (update k)
    updateGet k    = lift . (updateGet k)
    updateWhere f  = lift . (updateWhere f)
    deleteWhere    = lift . deleteWhere
    selectSource f = (C.transPipe lift) . (selectSource f)
    selectFirst f  = lift . (selectFirst f)
    selectKeys f   = (C.transPipe lift) . (selectKeys f)
    count          = lift . count




