{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module LiveUtils (
    liveTest,
    listTables,
    checkTableExists
) where

    import qualified Control.Exception                as Ex
    import qualified Data.Text                        as Text
    import           Data.Time.Clock.POSIX            (getPOSIXTime)
    import qualified Database.PostgreSQL.Simple       as PG
    import           Database.PostgreSQL.Simple.SqlQQ
    import qualified Database.PostgreSQL.Simple.Types as PG
    import           Test.HUnit

    liveTest :: (PG.Connection -> Assertion) -> PG.ConnectInfo -> Test
    liveTest act baseInfo =
            let go :: Assertion
                go = withDatabase baseInfo
                        (\liveInfo -> withConnection liveInfo act)
            in
            TestCase go

    withDatabase :: forall a. PG.ConnectInfo -> (PG.ConnectInfo -> IO a) -> IO a
    withDatabase baseInfo act = Ex.bracket makeDB dropDB go
        where
            makeDB :: IO Text.Text
            makeDB = do
                now <- getPOSIXTime
                let dbname :: Text.Text
                    dbname = Text.pack $ "psql-migrate-testdb-" ++ show now
                withConnection baseInfo $ \conn -> do
                    _ <- PG.execute conn "CREATE DATABASE ?;"
                            (PG.Only (PG.Identifier dbname))
                    pure dbname

            dropDB :: Text.Text -> IO ()
            dropDB dbname = do
                r :: Either Ex.SomeException ()
                    <- Ex.try $
                        withConnection baseInfo $ \conn -> do
                            _ <- PG.execute conn "DROP DATABASE ?;"
                                    (PG.Only (PG.Identifier dbname))
                            pure ()
                case r of
                    Right () -> pure ()
                    Left (Ex.SomeException e) -> do
                        putStrLn $
                            "Error dropping database "
                            ++ showInfo
                                (baseInfo
                                    { PG.connectDatabase =
                                        Text.unpack dbname })
                            ++ ": " ++ show e
                        putStrLn
                            "This database may need to be dropped by hand."
                        Ex.throw e

            showInfo :: PG.ConnectInfo -> String
            showInfo cinfo = PG.connectUser cinfo
                                ++ "@"
                                ++ PG.connectHost cinfo
                                ++ ":"
                                ++ show (PG.connectPort cinfo)
                                ++ "/"
                                ++ PG.connectDatabase cinfo
 
            go :: Text.Text -> IO a
            go dbname =
                act $ baseInfo { PG.connectDatabase = Text.unpack dbname }

    withConnection :: forall a .
                        PG.ConnectInfo
                        -> (PG.Connection -> IO a)
                        -> IO a
    withConnection cinfo act = Ex.bracket makeConn PG.close act
        where
            makeConn :: IO PG.Connection
            makeConn = do
                conn <- PG.connect cinfo
                _ :: [ PG.Only Int ] <- PG.query_ conn "SELECT 1;"
                pure conn

    listTables :: PG.Connection -> IO [ String ]
    listTables conn =
        fmap PG.fromOnly <$>
            PG.query_ conn
                    [sql|
                        SELECT
                            tablename
                        FROM
                            pg_tables
                        WHERE
                            schemaname = 'public';
                    |]

    checkTableExists :: PG.Connection -> String -> IO Bool
    checkTableExists conn table = do
        r <- PG.query conn
                [sql|
                    SELECT
                        EXISTS(
                            SELECT
                                1
                            FROM
                                pg_tables
                            WHERE
                                schemaname = 'public'
                                AND tablename = ?
                        );
                |]
                (PG.Only table)
        pure $
            case r of
                []              -> False
                (PG.Only x) : _ -> x


