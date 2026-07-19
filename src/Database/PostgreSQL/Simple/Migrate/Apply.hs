{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.Migrate.Apply (
    Verbosity(..),
    AdvisoryLock(..),
    Config(..),
    defaultConfig,
    apply,
) where

    import qualified Control.Exception                      as Ex
    import           Data.Foldable                          (traverse_)
    import           Data.Int                               (Int64)
    import           Data.Kind                              (Type)
    import           Data.Map.Strict                        (Map)
    import qualified Data.Map.Strict                        as Map
    import           Data.Set                               (Set)
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ       (sql)
    import qualified Database.PostgreSQL.Simple.Transaction as PG
    import qualified Database.PostgreSQL.Simple.Types       as PG

    -- These break formatting.
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig    as Mig
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Schema
        as Schema
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Valid
        as Valid

    data Verbosity = Verbose | Silent
        deriving stock (Show, Read, Ord, Eq, Enum, Bounded)

    type Verbosity :: Type

    data AdvisoryLock = BlockOnLock | TryLock | SkipLock
        deriving stock (Show, Read, Ord, Eq, Enum, Bounded)

    type AdvisoryLock :: Type

    data Config = Config {
        verbosity    :: Verbosity,
        advisoryLock :: AdvisoryLock }

    type Config :: Type

    defaultConfig :: Config
    defaultConfig = Config {
        verbosity     = Verbose,
        advisoryLock = BlockOnLock }

    apply :: PG.Connection
                -> Mig.Schema
                -> Config
                -> IO (Either String ())
    apply conn makeMigs config = do
            r :: Either Ex.SomeException () <- Ex.try core
            pure $ case r of
                        Left  (Ex.SomeException e) -> Left (show e)
                        Right ()                   -> Right ()
        where
            say :: String -> IO ()
            say msg = case verbosity config of
                        Silent  -> pure ()
                        Verbose -> putStrLn msg


            core :: IO ()
            core =
                takeLock $ do
                    applied :: Set Text <- Schema.initializeSchemaState conn
                    let schemaState :: Mig.SchemaState
                        schemaState = Mig.SchemaState {
                                        Mig.getAllApplied = applied,
                                        Mig.isUpgrading   = True }

                        migs :: [ Mig.Migration ]
                        migs = makeMigs schemaState

                    case Valid.validate schemaState migs of
                        Just err -> Ex.throwIO err
                        Nothing  -> do
                            let toApply :: [ Mig.Migration ]
                                toApply = filter
                                            (not . Mig.isApplied schemaState)
                                            toApply

                                order :: [ Mig.Migration ]
                                order = orderApplies toApply
                            traverse_ (applyMig (length order))
                                            (zip order [1..])

            applyMig :: Int -> (Mig.Migration, Int) -> IO ()
            applyMig n (mig, i) =
                case Mig.getAction mig of
                    Mig.Apply qry args -> do
                        say $ "Applying " ++ show (Mig.getName mig)
                                ++ "(" ++ show i ++ "/" ++ show n ++ ")"
                        PG.withTransactionLevel PG.Serializable conn $ do
                            _ <- PG.execute conn qry args
                            Schema.markApplied conn (Mig.getName mig)
                    Mig.Delete         -> do
                        say $ "Deleting " ++ show (Mig.getName mig)
                        Schema.deleteApplied conn (Mig.getName mig)
                    Mig.NoAction       -> do
                        say $ "Marking as complete " ++ show (Mig.getName mig)
                        Schema.markApplied conn (Mig.getName mig)


            takeLock :: forall a . IO a -> IO a
            takeLock go =
                    case advisoryLock config of
                        BlockOnLock -> Ex.bracket_ getLock releaseLock go
                        TryLock     -> Ex.bracket_ tryLock releaseLock go
                        SkipLock    -> go
                where
                    getLock :: IO ()
                    getLock = do
                        say "Waiting for advisory lock."
                        _ :: [ PG.Only PG.Null ]
                            <- PG.execute conn
                                [sql| SELECT pg_advisory_lock(?); |]
                                (PG.Only lockId)
                        say "Advisory lock acquired."
                        pure ()

                    tryLock :: IO ()
                    tryLock = do
                        say "Trying to acquire advisory lock."
                        r :: [ PG.Only Bool ]
                            <- PG.query conn
                                [sql| SELECT pg_try_advisory_lock(?); |]
                                (PG.Only lockId)
                        case r of
                            (PG.Only True) : _ ->
                                say "Advisory lock acquired."
                            _                  ->
                                error "Failed to acquire advisory lock."

                    releaseLock :: IO ()
                    releaseLock = do
                        _ :: [ PG.Only PG.Null ] <- PG.query conn
                                [sql| SELECT pg_advisory_unlock(?); |]
                                (PG.Only lockId)
                        say "Advisory lock released."
                        pure ()

                    -- This is the lock id used by node-pg-migrate.
                    -- Which I hope is some form of standard, well-known
                    -- identifier.
                    lockId :: Int64
                    lockId = 7241865325823964


    orderApplies :: [ Mig.Migration ] -> [ Mig.Migration ]
    orderApplies migList =
            foldr
                go
                (const [])
                (Mig.getName <$> migList)
                initMigs

        where
            initMigs :: Map Text Mig.Migration
            initMigs = Map.fromList $ (\m -> (Mig.getName m, m)) <$> migList

            go :: Text
                    -> (Map Text Mig.Migration -> [ Mig.Migration ])
                    -> Map Text Mig.Migration
                    -> [ Mig.Migration ]
            go name rest migMap =
                case Map.lookup name migMap of
                    -- Not finding ourselves in the map is to be expected-
                    -- what this means is that we've already been added
                    -- to the list before we got here, so we can just
                    -- skip this migration now.
                    -- It can also mean it's a dependency that was
                    -- already installed in the database, so again we
                    -- can just skip it.
                    Nothing  -> rest migMap
                    Just mig ->
                        -- make sure all of our dependencies are in
                        -- the list before we add ourselves
                        foldr
                            go
                            (\migMap2 -> mig : rest migMap2)
                            (Mig.getDependencies mig)
                            (Map.delete (Mig.getName mig) migMap)

