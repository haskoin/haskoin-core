module Bitcoin.Protocol.Tests ( tests ) where

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import QuickCheckUtils

import Bitcoin.Protocol
import Bitcoin.Protocol.VarInt
import Bitcoin.Protocol.VarString
import Bitcoin.Protocol.NetworkAddress
import Bitcoin.Protocol.Version
import Bitcoin.Protocol.Addr

tests =
    [ testGroup "Serialize/Deserialize Protocol Types"
        [ testProperty "ident VarInt" (meta_ident :: VarInt -> Bool)
        , testProperty "ident VarString" (meta_ident :: VarString -> Bool)
        , testProperty "ident NetworkAddress" 
            (meta_ident :: NetworkAddress -> Bool)
        , testProperty "ident Version" (meta_ident :: Version -> Bool)
        , testProperty "ident Addr" (meta_ident :: Addr -> Bool)
        ]
    ]

meta_ident t = runGet bitcoinGet (runPut . bitcoinPut $ t) == t

