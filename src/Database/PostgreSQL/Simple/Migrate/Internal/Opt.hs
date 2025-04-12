{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Opt
-- Description : Functions for handing optional migrations
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
module Database.PostgreSQL.Simple.Migrate.Internal.Opt (
    migrationIsApplied,
    appliedOptionalMigrations
) where

    import qualified Data.CaseInsensitive             as CI
    import           Data.Text                        (Text)
    import qualified Database.PostgreSQL.Simple       as PG
    import           Database.PostgreSQL.Simple.SqlQQ (sql)
    
    -- | Check if a specific migration has been applied.
    --
    -- The main use for this function is to help deal with optional
    -- migrations.  The idea is that the program can behave differently
    -- depending upon whether the transaction exists or not.
    --
    -- To test if any of several migrations have been applied, consider
    -- using `appliedOptionalMigrations`, which is significantly more
    -- efficient than repeatedly calling this function.
    --
    -- Note that the result of this function is only valid during
    -- the transaction it is called in.  Calling it when not in a
    -- transaction will \"work\"- PostgreSQL will wrap the function
    -- in a transaction.  But the schema could update a single
    -- nanosecond later.
    --
    -- The correct way to use this function is to start a transaction,
    -- call this function, and then do all actions that depend upon
    -- the result in the same transaction.
    migrationIsApplied :: PG.Connection
                            -> Text
                            -> IO Bool
    migrationIsApplied conn mig = do
            let mname :: Text
                mname = CI.foldedCase $ CI.mk mig
            res :: [ PG.Only Bool ] <- 
                PG.query conn
                    [sql|
                        SELECT EXISTS (
                            SELECT 1 FROM schema_migrations
                                WHERE name = ?
                                LIMIT 1);
                    |]
                    (PG.Only mname)
            pure $
                case res of
                    []              -> False
                    (PG.Only x : _) -> x

    -- | Return the list of migrations that optional, but have been applied.
    --
    -- It behaves as if `migrationIsApplied` is called for each migration,
    -- and the migration is returned if that function returns true.  But
    -- it is significantly more efficient in that it only performs one
    -- query to the database.
    --
    -- Like `migrationIsApplied`, this function should only be called in
    -- a transaction.
    appliedOptionalMigrations :: PG.Connection
                                    -> [ Text ]
                                    -> IO [ Text ]
    appliedOptionalMigrations conn migs = do
        let mnames :: [ Text ]
            mnames = fmap (CI.foldedCase . CI.mk) migs
        res :: [ PG.Only Text ] <-
            PG.query conn
                [sql| SELECT DISTINCT name
                        FROM schema_migrations
                        WHERE name IN ?; |]
                (PG.Only (PG.In mnames))
        pure $ PG.fromOnly <$> res


