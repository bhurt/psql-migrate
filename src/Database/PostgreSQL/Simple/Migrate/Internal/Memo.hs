{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.PostgreSQL.Simple.Migrate.Internal.Memo (
    MemoT,
    runMemoT,
    getMemo,
    getCached
) where

    import qualified Control.Monad.Except   as Except
    import           Control.Monad.IO.Class (MonadIO, liftIO)
    import qualified Control.Monad.Reader   as Reader
    import qualified Control.Monad.State    as State
    import           Control.Monad.Trans    (MonadTrans, lift)
    import           Data.Kind              (Type)
    import           Data.Map.Strict        (Map)
    import qualified Data.Map.Strict        as Map

    newtype MemoT k v m a =
        MemoT {
            getMemoT :: Reader.ReaderT (k -> MemoT k v m v)
                            (State.StateT (Map k v) m) a }
        deriving newtype (Functor, Applicative, Monad, MonadFail)

    type MemoT :: Type -> Type -> (Type -> Type) -> Type -> Type

    instance MonadIO m => MonadIO (MemoT k v m) where
        liftIO = MemoT . liftIO

    instance MonadTrans (MemoT k v) where
        lift = MemoT . lift . lift

    myLocal :: forall k v m a r .
                (Reader.MonadReader r m)
                => (r -> r)
                -> MemoT k v m a
                -> MemoT k v m a
    myLocal f m0 = MemoT $ do
        go :: k -> MemoT k v m v <- Reader.ask
        let m1 :: Reader.ReaderT (k -> MemoT k v m v)
                            (State.StateT (Map k v) m) a 
            m1 = getMemoT m0

            m2 :: State.StateT (Map k v) m a
            m2 = Reader.runReaderT m1 go

            m3 :: State.StateT (Map k v) m a
            m3 = Reader.local f m2

        lift m3
        
    instance Reader.MonadReader r m => Reader.MonadReader r (MemoT k v m) where
        ask = MemoT . lift . lift $ Reader.ask
        reader = MemoT . lift . lift . Reader.reader
        local = myLocal

    instance Except.MonadError e m => Except.MonadError e (MemoT k v m) where
        throwError = MemoT . lift . lift . Except.throwError
        catchError act cat =
            MemoT $ Except.catchError (getMemoT act) (getMemoT . cat)

    instance State.MonadState s m => State.MonadState s (MemoT k v m) where
        get = MemoT . lift . lift $ State.get
        put = MemoT . lift . lift . State.put

    runMemoT :: forall k v m a . 
                    Monad m
                    => (k -> MemoT k v m v)
                    -> MemoT k v m a
                    -> m a
    runMemoT go memo =
        let m1 :: Reader.ReaderT (k -> MemoT k v m v)
                        (State.StateT (Map k v) m) a
            m1 = getMemoT memo

            m2 :: State.StateT (Map k v) m a
            m2 = Reader.runReaderT m1 go 
        in
        State.evalStateT m2 Map.empty


    getMemo :: forall k v m .
                (Monad m
                , Ord k)
                => k
                -> MemoT k v m v
    getMemo k = MemoT $ do
        mp :: Map k v <- lift $ State.get
        case Map.lookup k mp of
            Just v  -> pure v
            Nothing -> do
                go :: k -> MemoT k v m v <- Reader.ask
                v <- getMemoT $ go k
                lift $ State.modify' (Map.insert k v) 
                pure v

    getCached :: forall k v m .
                    (Monad m
                    , Ord k)
                    => k
                    -> MemoT k v m (Maybe v)
    getCached k = MemoT $ do
        mp :: Map k v <- lift $ State.get
        pure $ Map.lookup k mp

