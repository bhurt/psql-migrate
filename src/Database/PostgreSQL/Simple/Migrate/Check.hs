
module Database.PostgreSQL.Simple.Migrate.Check (
    check
) where

    import qualified Control.Exception          as Ex
    import           Data.Set                   (Set)
    import           Data.Text                  (Text)
    import qualified Database.PostgreSQL.Simple as PG

    -- These break formatting.
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig
        as Mig
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Schema
        as Schema


    check :: PG.Connection
                -> Mig.Schema
                -> IO (Either String ())
    check conn makeMigs = do
            r :: Either Ex.SomeException (Maybe String) <- Ex.try core
            pure $ case r of
                        Left  (Ex.SomeException e) -> Left (show e)
                        Right (Just err)           -> Left err
                        Right Nothing              -> Right ()
        where
            core :: IO (Maybe String)
            core = do
                mapplied :: Maybe (Set Text)
                    <- Schema.checkingSchemaState conn
                case mapplied of
                    Nothing -> pure $ Just "Database is uninitialized"
                    Just applied -> pure $
                        let schemaState :: Mig.SchemaState
                            schemaState = Mig.SchemaState {
                                            Mig.getAllApplied = applied,
                                            Mig.isUpgrading   = False }

                            migs :: [ Mig.Migration ]
                            migs = makeMigs schemaState
                        in
                        foldr (go schemaState) Nothing migs

            go :: Mig.SchemaState
                    -> Mig.Migration
                    -> Maybe String
                    -> Maybe String
            go schemaState mig rest =
                if Mig.isApplied schemaState mig
                then rest
                else Just $ "Unapplied migration: " ++ show mig

