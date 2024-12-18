{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Apply
-- Description : Apply migrations to a database.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of "Database.PostgreSQL.Simple.Migrate",
-- you probably want that module instead.  Anything exported by this
-- module that is not also exported by the main module is subject
-- to change without notice.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Apply (
    apply,
    check
) where

    import qualified Control.Exception                      as Exception
    import           Control.Monad                          (when)
    import qualified Data.CaseInsensitive                   as CI
    import qualified Data.Foldable                          as Foldable
    import           Data.Int                               (Int64)
    import           Data.String
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Transaction as PG
    import qualified Database.PostgreSQL.Simple.Types       as PG

    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import Database.PostgreSQL.Simple.Migrate.Internal.Error
    import Database.PostgreSQL.Simple.Migrate.Internal.Order
    import Database.PostgreSQL.Simple.Migrate.Internal.Types

    -- | Apply a set of migrations to a database.
    --
    -- If the "Database.PostgreSQL.Simple" library throws an exception,
    -- it is rethrown by this function.  Otherwise, if there is an
    -- error, the error printed and the application of migrations
    -- is halted.
    --
    apply :: forall logmsg .
                (IsString logmsg
                , Semigroup logmsg)

                => (Verbose -> IO logmsg -> IO ())
                -- ^ How to print out status messages.
                --
                -- This allows for fancy loging.

                -> [ Migration ]
                -- ^ The list of migrations to apply.

                -> PG.Connection
                -- ^ The database connection.
                --

                -> Bool
                -- ^ Whether to block waiting for the advisory lock
                --
                -- If True, this function will block indefinitely, waiting
                -- to acquire the advisory lock.  If Flase, this function
                -- will return the `MigrationsError` value `Locked`.

                -> IO (Either MigrationsError ())
                -- ^ Returns a `MigrationsError` if there was an error.
                --
                -- Use 
                -- `Database.PostgreSQL.Simple.Migrate.formatMigrationsError`
                -- to convert an error to a printable string.
                --
                -- Done this way so that it's easy to cast to an:
                --
                -- @
                -- ExceptT MigrationsError IO ()
                -- @
                --
                -- mtl stack.
                --
    apply _      []   _    _     = pure $ Left EmptyMigrationList
    apply logmsg migs conn block = Exception.try $ do
        withLock logmsg conn block $ do
            logmsg Low $ pure "Checking if the database is initialized."
            initialized <- isInitialized logmsg conn
            aps :: [ (Text, Text) ]
                <- if not initialized
                    then do
                        logmsg Low $
                            pure "Initializing database."
                        initialize logmsg conn
                        pure []
                    else do
                        logmsg Low $ pure "Database is initialized."
                        logmsg Low . pure $
                            "Fetching already applied migrations."
                        getAppliedMigrations logmsg conn
            logmsg Low $ pure "Ordering migrations."
            let ord :: Either
                            MigrationsError
                            (Maybe Migration, [ (Apply, Migration) ])
                ord = orderMigrations migs aps
            case ord of
                Left err       -> Exception.throw err
                Right (_, [])  -> do
                    logmsg Low $ pure "No migrations need to be applied."
                Right (_, mgs) -> do
                    logmsg Low $ pure "Applying migrations."
                    _ <- Foldable.foldlM
                            (applyMigration logmsg conn)
                            Nothing
                            mgs
                    pure ()
            logmsg Low $ pure "Complete."
            pure ()

    -- | Check that a set of migrations has been applied to a database.
    --
    -- This function is similar to `apply`, except that no migrations
    -- are applied.  Instead, the database is checked for correctness.
    -- Note that optional migrations need not have been applied.
    --
    -- If the "Database.PostgreSQL.Simple" library throws an exception,
    -- it is rethrown by this function.  Otherwise, if there is an
    -- error, the error printed and the checking of migrations
    -- is halted.
    --
    check :: forall logmsg .
                (IsString logmsg
                , Semigroup logmsg)

                => (Verbose -> IO logmsg -> IO ())
                -- ^ How to print out status messages.
                --
                -- This allows for fancy loging.

                -> [ Migration ]
                -- ^ The list of migrations to check.

                -> PG.Connection
                -- ^ The database connection.
                --

                -> Bool
                -- ^ Whether to block waiting for the advisory lock
                --
                -- If True, this function will block indefinitely, waiting
                -- to acquire the advisory lock.  If Flase, this function
                -- will return the `MigrationsError` value `Locked`.

                -> IO (Either MigrationsError ())
                -- ^ Returns a MigrationsError if the database is not
                -- consistent with the given list of migrations.
                --
                -- Use 
                -- `Database.PostgreSQL.Simple.Migrate.formatMigrationsError`
                -- to convert an error to a printable string.
                --
                -- Done this way so that it's easy to cast to an:
                --
                -- @
                -- ExceptT MigrationsError IO ()
                -- @
                --
                -- mtl stack.
                --
    check _      []   _    _     = pure $ Left EmptyMigrationList
    check logmsg migs conn block = Exception.try $ do
        aps :: [ (Text, Text) ] <-
            withLock logmsg conn  block $ do
                logmsg Low $ pure "Checking if the database is initialized."
                initialized <- isInitialized logmsg conn
                if initialized
                then do
                    logmsg Low $ pure "Database is initialized."
                    logmsg Low $ pure "Fetching already applied migrations."
                    getAppliedMigrations logmsg conn
                else Exception.throw Uninitialized
        logmsg Low $ pure "Ordering migrations."
        let ord :: Either
                        MigrationsError
                        (Maybe Migration, [ (Apply, Migration) ])
            ord = orderMigrations migs aps
        case ord of
            Left err        -> Exception.throw err
            Right (mmig, _) ->
                case mmig of
                    Just mig -> Exception.throw $ RequiredUnapplied mig
                    Nothing  -> pure ()
        logmsg Low $ pure "Complete."
        pure ()


    -- | Acquire and hold a PosgreSQL advisory lock, to synchronize updates to
    -- the database schema.
    --
    -- Note: we release the lock when we exit.  But even if the program
    -- exits without cleanup (i.e. via a SIGKILL or the like), the advisory
    -- lock will be released when the pgsql session is closed (i.e. when
    -- the TCP connection is closed).
    --
    withLock :: forall logmsg a .
                (IsString logmsg
                , Semigroup logmsg)
                => (Verbose -> IO logmsg -> IO ())
                -> PG.Connection
                -> Bool
                -> IO a
                -> IO a
    withLock logmsg conn block act =
            Exception.bracket lock unlock (\() -> act)
        where
            lock :: IO ()
            lock = do
                logmsg Medium (pure "Acquiring database advisory lock.")
                if block
                then do
                    _ :: [ PG.Only PG.Null ]
                        <- pgQuery logmsg conn
                            [sql| SELECT pg_advisory_lock(?); |]
                            (PG.Only lockId)
                    pure ()
                else do
                    r :: [ PG.Only Bool ]
                        <- pgQuery logmsg conn
                            [sql| SELECT pg_try_advisory_lock(?); |]
                            (PG.Only lockId)
                    case r of
                        (PG.Only True) : _ -> pure ()
                        _                  -> Exception.throw Locked
                logmsg Medium (pure "Lock acquired.")


            unlock :: () -> IO ()
            unlock () = do
                -- Do our best to unlock the advisory lock.  If this
                -- fails, do not throw an exception.
                logmsg Medium (pure "Releasing database advisory lock.")
                _ :: Either Exception.SomeException [ PG.Only PG.Null ]
                     <- Exception.try $
                        pgQuery logmsg conn
                            [sql| SELECT pg_advisory_unlock(?); |]
                            (PG.Only lockId)
                pure ()

            -- This is the lock id used by node-pg-migrate.  Which I hope
            -- is some form of standard, well-known identifier.
            lockId :: Int64
            lockId = 7241865325823964

    -- | Check to see if we are initialized.
    --
    -- I.e. that the schema table has been created.
    isInitialized :: forall logmsg .
                    (IsString logmsg
                    , Semigroup logmsg)
                    => (Verbose -> IO logmsg -> IO ())
                    -> PG.Connection
                    -> IO Bool
    isInitialized logmsg conn = do
        r1 :: [ PG.Only Bool ]
            <- pgQuery logmsg conn
                [sql|   SELECT EXISTS (
                            SELECT 1
                            FROM pg_tables
                            WHERE tablename = 'schema_migrations'
                                AND schemaname = 'public'
                            LIMIT 1
                        ); |]
                ()
        pure $
            case r1 of
                (PG.Only True) : _ -> True
                _                  -> False


    -- | Initialize the database.
    --
    -- I.e. create the schema table.
    --
    initialize :: forall logmsg .
                    (IsString logmsg
                    , Semigroup logmsg)
                    => (Verbose -> IO logmsg -> IO ())
                    -> PG.Connection
                    -> IO ()
    initialize logmsg conn =
        PG.withTransactionLevel PG.Serializable conn $ do
            _ <- pgExecute logmsg conn
                [sql|
                    CREATE TABLE IF NOT EXISTS schema_migrations(
                        name TEXT PRIMARY KEY,
                        fingerprint CHAR(44) UNIQUE NOT NULL,
                        executed_at TIMESTAMP WITH TIME ZONE
                            NOT NULL DEFAULT NOW());
                |] ()
            _ <- pgExecute logmsg conn
                [sql|
                    CREATE INDEX ON schema_migrations(executed_at);
                |] ()
            pure ()


    -- | Read the list of already applied migrations (and their fingerprints)
    -- from the database.
    getAppliedMigrations :: forall logmsg .
                                (IsString logmsg
                                , Semigroup logmsg)
                                => (Verbose -> IO logmsg -> IO ())
                                -> PG.Connection
                                -> IO [ (Text, Text) ]
    getAppliedMigrations logmsg conn =
        pgQuery logmsg conn
            [sql| SELECT (name, fingerprint) FROM schema_migrations; |] ()

    -- | Apply a migration.
    applyMigration :: forall logmsg .
                        (IsString logmsg
                        , Semigroup logmsg)
                        => (Verbose -> IO logmsg -> IO ())
                        -> PG.Connection
                        -> Maybe Int
                        -> (Apply, Migration)
                        -> IO (Maybe Int)
    applyMigration logmsg conn mphase (app, mig) = do
        let newPhase :: Bool
            newPhase = case mphase of
                            Nothing -> True
                            Just p  -> p < phase mig
        when newPhase $ logmsg Medium 
                            (pure $ "Starting phase "
                                        <> fromString (show (phase mig)))
        logmsg High (pure $ "Applying migration "
                                <> fromString (showMigration mig))
        r <- Exception.try $ 
            PG.withTransactionLevel PG.Serializable conn $ do
                case app of
                    Apply   -> do
                        _ <- pgExecute logmsg conn (command mig) ()
                        pure ()
                    Replace -> do
                        _ <- pgExecute logmsg conn
                                [sql| DELETE FROM schema_migrations
                                        WHERE name IN ?; |]
                                (PG.Only
                                    (PG.In
                                        (fmap
                                            (CI.foldedCase . rName)
                                            (replaces mig))))
                        pure ()
                _ <- pgExecute logmsg conn
                    [sql| INSERT INTO schema_migrations
                            (name, fingerprint) VALUES (?, ?); |]
                    (CI.foldedCase (name mig), fingerprint mig)
                pure ()
        case r of
            Right ()                         -> pure $ Just (phase mig)
            Left (Exception.SomeException e) -> do
                logmsg Low (pure $ fromString $
                                "Exception thrown in apply migration "
                                ++ showMigration mig ++ ": " ++ show e)
                Exception.throw e


    pgQuery :: forall logmsg q r .
                (IsString logmsg
                , Semigroup logmsg
                , PG.ToRow q
                , PG.FromRow r)
                => (Verbose -> IO logmsg -> IO ())
                -> PG.Connection
                -> PG.Query
                -> q
                -> IO [ r ]
    pgQuery logmsg conn qry q = do
        logmsg Detail $ do
            bs <- PG.formatQuery conn qry q
            pure $ "Executing: " <> fromString (show bs)
        PG.query conn qry q

    pgExecute :: forall logmsg q .
                (IsString logmsg
                , Semigroup logmsg
                , PG.ToRow q)
                => (Verbose -> IO logmsg -> IO ())
                -> PG.Connection
                -> PG.Query
                -> q
                -> IO Int64
    pgExecute logmsg conn qry q = do
        logmsg Detail $ do
            bs <- PG.formatQuery conn qry q
            pure $ "Executing: " <> fromString (show bs)
        PG.execute conn qry q

