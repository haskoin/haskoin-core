module Network.Haskoin.Wallet.Types.DeriveJSON 
( dropFieldLabel 
, dropSumLabels
) where

import Data.Char (toLower)
import Data.Aeson.Types 
    ( Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
    )

dropFieldLabel :: Int -> Options
dropFieldLabel n = defaultOptions
    { fieldLabelModifier = map toLower . drop n
    , omitNothingFields  = True
    } 

dropSumLabels :: Int -> Int -> String -> Options
dropSumLabels c f tag = (dropFieldLabel f)
    { constructorTagModifier = map toLower . drop c
    , sumEncoding = defaultTaggedObject { tagFieldName = tag }
    } 

