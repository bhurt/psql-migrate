{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.PostgreSQL.Simple.Migrate.Internal.Schema (
    initializeSchemaState,
    checkingSchemaState,
    markApplied,
    deleteApplied,
    isApplied
) where

    import           Control.Monad                          (unless)
    import           Data.Set                               (Set)
    import qualified Data.Set                               as Set
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ       (sql)
    import qualified Database.PostgreSQL.Simple.Transaction as PG
    import qualified Database.PostgreSQL.Simple.Types       as PG

    rawSchemaName :: Text
    rawSchemaName = "psqlmigrate"

    rawTableName :: Text
    rawTableName = "migrations"

    tableName :: PG.QualifiedIdentifier
    tableName = PG.QualifiedIdentifier
                    (Just rawSchemaName)
                    rawTableName

    initializeSchemaState :: PG.Connection -> IO (Set Text)
    initializeSchemaState conn =
        PG.withTransactionLevel PG.Serializable conn $ do
            schemaExists :: Bool <- lookForSchema conn
            unless schemaExists $ createSchema conn
            tableExists :: Bool
                <- if schemaExists
                    then lookForTable conn
                    else pure False
            unless tableExists $ createTable conn
            if tableExists
            then readTable conn
            else pure Set.empty

    checkingSchemaState :: PG.Connection -> IO (Maybe (Set Text))
    checkingSchemaState conn =
        let mode :: PG.TransactionMode
            mode = PG.TransactionMode {
                    PG.isolationLevel = PG.Serializable,
                    PG.readWriteMode = PG.ReadOnly }
        in
        PG.withTransactionMode mode conn $ do
            schemaExists :: Bool <- lookForSchema conn
            tableExists :: Bool <- 
                if schemaExists
                then lookForTable conn
                else pure False
            if tableExists
            then Just <$> readTable conn
            else pure Nothing

    lookForSchema :: PG.Connection -> IO Bool
    lookForSchema conn = do
        r :: [ PG.Only Bool ]
            <- PG.query conn [sql|
                    SELECT EXISTS(
                        SELECT 1
                        FROM information_schema.schemata
                        WHERE schema_name = ?); |]
                    (PG.Only rawSchemaName)
        pure $
            case r of
                (PG.Only True) : _ -> True
                _                  -> False

    createSchema :: PG.Connection -> IO ()
    createSchema conn = do
        _ <- PG.execute conn [sql|
                CREATE SCHEMA IF NOT EXISTS ?; |]
                (PG.Only (PG.Identifier rawSchemaName))
        pure ()

    lookForTable :: PG.Connection -> IO Bool
    lookForTable conn = do
        r :: [ PG.Only Bool ] <- PG.query conn [sql|
            SELECT EXISTS (
                SELECT 1
                FROM information_schema.tables
                WHERE
                    table_name = ?
                    AND schema_name = ?); |]
            (rawTableName, rawSchemaName)
        pure $ case r of
                (PG.Only True) : _ -> True
                _                  -> False

    createTable :: PG.Connection -> IO ()
    createTable conn = do
        _ <- PG.execute conn [sql|
                CREATE TABLE ? (name TEXT PRIMARY KEY); |]
                (PG.Only tableName)
        pure ()

    readTable :: PG.Connection -> IO (Set Text)
    readTable conn =
        Set.fromList . fmap PG.fromOnly <$>
            PG.query conn
                [sql| SELECT (name) FROM ?; |]
                (PG.Only tableName)

    markApplied :: PG.Connection -> Text -> IO ()
    markApplied conn name = do
        _ <- PG.execute conn [sql| INSERT INTO ? (name) VALUES (?); |]
                (tableName, name)
        pure ()

    deleteApplied :: PG.Connection -> Text -> IO ()
    deleteApplied conn name = do
        _ <- PG.execute conn [sql| DELETE FROM ? WHERE name = ?; |]
                (tableName, name)
        pure ()

    isApplied :: PG.Connection -> Text -> IO Bool
    isApplied conn name = do
        r :: [ PG.Only Bool ]
            <- PG.query conn
                [sql| SELECT EXISTS(SELECT 1 FROM ? WHERE name = ?); |]
                (tableName, name)
        case r of
            []              -> error "Impossible state reached."
            (PG.Only b) : _ -> pure b

