--import Prettify2
import Test.QuickCheck.Batch

options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }

main = do
    runTests "simple" options
        [ run prop_identity
        ]

