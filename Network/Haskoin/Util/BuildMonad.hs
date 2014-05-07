{-|
  The Build type, and associated operations.
-}
module Network.Haskoin.Util.BuildMonad 
( -- *Build monad
  Build(..)
, isComplete
, isPartial
, isBroken
, eitherToBuild
, buildToEither
, guardPartial

  -- *BuildT transformer monad
, BuildT(..)
, liftBuild
) where

import Control.Monad (liftM)
import Control.Monad.Trans 
    ( MonadTrans
    , MonadIO
    , lift
    , liftIO
    )

{-|
  The Build monad represents computations that can be in one of three states:
  
      * Complete

      * Partial

      * Broken

  It extends the Either monad with an additional Partial value to describe a
  valid computation flagged with a Partial context. The Build monad is useful
  when you describe computations where parts of the computation are either
  complete, partially complete or broken. Combining only Complete computations
  will produce a Complete result. However, if one of the computations is
  Partial, the whole computation will be Partial as well. And if some
  computation is Broken, the whole computation will be broken as well.

  The Build monad is used by Haskoin to describe the state of the transaction
  signing computation. To sign a transaction, all input scripts need to be 
  signed. The whole transaction will be completely signed only if all the
  input scripts are completely signed. If any of the inputs is partially signed,
  then the whole transaction will be partially signed as well. And the whole
  transaction is broken if one of the inputs failed to parse or is broken.
-}
data Build a 
    -- | Describes a successful complete computation
    = Complete { runBuild :: a } 
    -- | Describes a successful but partial computation
    | Partial  { runBuild :: a }
    -- | Describes a broken computation
    | Broken   { runBroken  :: String }
    deriving Eq

instance Show a => Show (Build a) where
    show (Complete a) = "Complete " ++ (show a)
    show (Partial a)  = "Partial " ++ (show a)
    show (Broken str) = "Broken " ++ str

instance Functor Build where
    fmap f (Complete x) = Complete (f x)
    fmap f (Partial x)  = Partial (f x)
    fmap _ (Broken s)   = Broken s

instance Monad Build where
    return = Complete
    Complete x >>= f = f x
    Partial x >>= f = case f x of
        e@(Broken _) -> e
        a            -> Partial $ runBuild a
    Broken s >>= _ = Broken s

-- | Returns True if the Build value is Complete
isComplete :: Build a -> Bool
isComplete (Complete _) = True
isComplete _            = False

-- | Returns True if the Build value is Partial
isPartial :: Build a -> Bool
isPartial (Partial _) = True
isPartial _           = False

-- | Return True if the Build value is Broken
isBroken :: Build a -> Bool
isBroken (Broken _) = True
isBroken _          = False

-- | Transforms an Either String value into a Build value. Right is mapped to
-- Complete and Left is mapped to Broken
eitherToBuild :: Either String a -> Build a
eitherToBuild m = case m of
    Left  err -> Broken err 
    Right res -> Complete res

-- | Transforms a Build value into an Either String value. Complete and Partial
-- are mapped to Right and Broken is mapped to Left.
buildToEither :: Build a -> Either String a
buildToEither m = case m of
    Complete a   -> Right a
    Partial  a   -> Right a
    Broken   err -> Left err

-- | Binds a Partial value to the computation when the predicate is False.
guardPartial :: Bool -> Build ()
guardPartial True  = Complete ()
guardPartial False = Partial ()

-- | BuildT transformer monad
newtype BuildT m a = BuildT { runBuildT :: m (Build a) }

mapBuildT :: (m (Build a) -> n (Build b)) -> BuildT m a -> BuildT n b
mapBuildT f = BuildT . f . runBuildT

instance Functor m => Functor (BuildT m) where
    fmap f = mapBuildT (fmap (fmap f))

instance Monad m => Monad (BuildT m) where
    return = lift . return
    x >>= f = BuildT $ do
        v <- runBuildT x
        case v of Complete a -> runBuildT (f a)
                  Partial a  -> runBuildT (f a)
                  Broken str -> return $ Broken str

instance MonadTrans BuildT where
    lift = BuildT . liftM Complete

instance MonadIO m => MonadIO (BuildT m) where
    liftIO = lift . liftIO

-- | Lift a Build computation into the BuildT monad
liftBuild :: Monad m => Build a -> BuildT m a
liftBuild = BuildT . return

