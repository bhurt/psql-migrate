{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.PostgreSQL.Simple.Migrate.Internal.Schema (
    SchemaState,
    makeSchemaState,

    getAllApplied,
    isInitializing,
    isUpgrading,
    isApplied,

    markApplied,
    deleteApplied

) where

    import           Data.Kind                              (Type)
    import           Data.Set                               (Set)
    import qualified Data.Set                               as Set
    import           Data.Text                              (Text)
    import qualified Database.PostgreSQL.Simple             as PG
    import           Database.PostgreSQL.Simple.SqlQQ       (sql)
    import qualified Database.PostgreSQL.Simple.Transaction as PG
    import qualified Database.PostgreSQL.Simple.Types       as PG

    data SchemaState = SchemaState {
        getAllApplied  :: Set Text,
        isInitializing :: Bool,
        isUpgrading    :: Bool
    }

    type SchemaState :: Type

    isApplied :: SchemaState -> Text -> Bool
    isApplied state name = Set.member name (getAllApplied state)

    rawSchemaName :: Text
    rawSchemaName = "psqlmigrate"

    rawTableName :: Text
    rawTableName = "migrations"

    tableName :: PG.QualifiedIdentifier
    tableName = PG.QualifiedIdentifier
                    (Just rawSchemaName)
                    rawTableName

    makeSchemaState :: PG.Connection -> Bool -> IO (Maybe SchemaState)
    makeSchemaState conn isUpgrading =
            PG.withTransactionLevel PG.Serializable conn $ do
                schemaExists <- lookForSchema
                if not schemaExists
                then
                    if isUpgrading
                    then do
                        createSchema
                        createTable
                        let getAllApplied :: Set Text
                            getAllApplied = Set.empty
                            isInitializing :: Bool
                            isInitializing = True
                        pure $ Just $ SchemaState { .. }
                    else pure Nothing
                else do
                    tableExists <- lookForTable
                    if not tableExists
                    then
                        if isUpgrading
                        then do
                            createTable
                            let getAllApplied :: Set Text
                                getAllApplied = Set.empty
                                isInitializing :: Bool
                                isInitializing = True
                            pure $ Just $ SchemaState { .. }
                        else pure Nothing
                    else do
                            getAllApplied :: Set Text <- readTable
                            let isInitializing :: Bool
                                isInitializing = False
                            pure $ Just $ SchemaState { .. }

        where
            lookForSchema :: IO Bool
            lookForSchema = do
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

            createSchema :: IO ()
            createSchema = do
                _ <- PG.execute conn [sql|
                        CREATE SCHEMA IF NOT EXISTS ?; |]
                        (PG.Only (PG.Identifier rawSchemaName))
                pure ()

            lookForTable :: IO Bool
            lookForTable = do
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

            createTable :: IO ()
            createTable = do
                _ <- PG.execute conn [sql|
                        CREATE TABLE ? (name TEXT PRIMARY KEY); |]
                        (PG.Only tableName)
                pure ()

            readTable :: IO (Set Text)
            readTable =
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

