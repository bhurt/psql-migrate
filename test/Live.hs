{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Live (
    withLiveTests
) where


    import qualified Control.Exception                  as Ex
    import qualified Database.PostgreSQL.Simple         as PG
    import           Database.PostgreSQL.Simple.Migrate
    import           Database.PostgreSQL.Simple.SqlQQ
    import           LiveUtils
    import           System.Exit
    import           Test.HUnit

    withLiveTests :: PG.ConnectInfo -> Test -> IO ()
    withLiveTests origInfo otherTests = do
            res :: Either Ex.SomeException Counts <- Ex.try go1
            case res of
                Left (Ex.SomeException e) -> do
                    putStrLn $ "Exception caught; " ++ show e
                    exitFailure
                Right cnts
                    | ((errors cnts > 0) || (failures cnts > 0)) ->
                        exitFailure
                    | otherwise -> exitSuccess

        where
            allTests :: Test
            allTests = TestList [ otherTests, liveTests origInfo ]

            go1 :: IO Counts
            go1 = do
                putStrLn ""
                r <- runTestTT allTests
                putStrLn ""
                pure r


    migFoo :: Migration
    migFoo = makeMigration "foo-1"
                [sql| CREATE TABLE foo (id SERIAL PRIMARY KEY); |]

    {-
    migBar :: Migration
    migBar = makeMigration "bar-1"
                [sql| CREATE TABLE foo (id SERIAL PRIMARY KEY); |]

    migFoo2 :: Migration
    migFoo2 = makeMigration "foo-2"
                [sql| ALTER TABLE foo ADD COLUMN foo VARCHAR; |]
                `addDependency` "foo-1"
    -}

    liveTests :: PG.ConnectInfo -> Test
    liveTests cinfo =
        let lst :: [ Test ]  = [ test1 cinfo ] in
        TestLabel "LiveTests" $ TestList lst

    {-
    noLogging :: Verbose -> IO String -> IO ()
    noLogging _ mkmsg = mkmsg >>= putStrLn
    -}

    noLogging :: Verbose -> IO String -> IO ()
    noLogging _ _ = pure ()

    test1 :: PG.ConnectInfo -> Test
    test1 = liveTest $ \conn -> do
                b1 :: Bool <- checkTableExists conn "foo"
                assert (not b1)
                res <- apply noLogging [ migFoo ] conn False
                case res of
                    Right () -> assert True
                    Left err -> do
                        putStrLn $ formatMigrationsError err
                        assert False
                b2 :: Bool <- checkTableExists conn "foo"
                assert b2

