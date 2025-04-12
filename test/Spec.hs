{-# LANGUAGE ScopedTypeVariables #-}

import qualified Database.PostgreSQL.Simple as PG
import qualified System.Environment         as Env
import           System.Exit                (die)
import           Test.HUnit                 (runTestTTAndExit)
import           Tests                      (tests)
import qualified Text.Read                  as Read
import qualified Live

main :: IO ()
main = do
        putStrLn $
            "Reading the environment to determine if we should "
            ++ "run live database tests\n(Because passing command"
            ++ "line arguments to the test program is clunky at best).\n"
        cfg :: Maybe PG.ConnectInfo <- getConnectInfo
        case cfg of
            Nothing -> runTestTTAndExit tests
            Just cinfo -> Live.withLiveTests cinfo tests

getConnectInfo :: IO (Maybe PG.ConnectInfo)
getConnectInfo = do
    f :: Maybe String <- getEnv "MIGRATE_TEST"
    case f of
        Nothing -> do
            putStrLn $ "Environment variable not set, not doing the live "
                        ++ "database tests.\n"
                        ++ "\n"
                        ++ "Consider re-running like:\n"
                        ++ "MIGRATE_TEST=\"\" stack test"
                        ++ " --flag psql-migrate:devel\n"
                        ++ "to enable them.\n"
            pure Nothing
        Just _ -> do
            putStrLn $ "Querying the environment for the database to "
                        ++ "connect to.\n"
            fmap Just $ do
                host <- getEnvStr "PGHOST"
                            (PG.connectHost PG.defaultConnectInfo)
                putStrLn ""
                port <- getEnvRead "PGPORT"
                            (PG.connectPort PG.defaultConnectInfo)
                putStrLn ""
                user <- getEnvStr "PGUSER"
                            (PG.connectUser PG.defaultConnectInfo)
                putStrLn ""
                pword <- getEnvStr "PGPASSWORD"
                            (PG.connectPassword PG.defaultConnectInfo)
                putStrLn ""
                dbname <- getEnvStr "PGDATABASE"
                            (PG.connectDatabase PG.defaultConnectInfo)
                putStrLn ""
                pure $ PG.ConnectInfo {
                            PG.connectHost = host,
                            PG.connectPort = port,
                            PG.connectUser = user,
                            PG.connectPassword = pword,
                            PG.connectDatabase = dbname }

getEnv :: String -> IO (Maybe String)
getEnv name = do
    putStrLn $ "Looking for environment variable " ++ show name
    res <- Env.lookupEnv name
    case res of
        Nothing -> putStrLn $ "Did not find variable " ++ show name
        Just val -> putStrLn $ "Found variable " ++ show name
                                    ++ " with value " ++ show val
    pure res

getEnvStr :: String -> String -> IO String
getEnvStr name defval = do
    res <- getEnv name
    case res of
        Nothing -> do
            putStrLn $ "Using default value for " ++ show name
                        ++ " of " ++ show defval
            pure defval
        Just val -> pure val

getEnvRead :: (Read a, Show a) => String -> a -> IO a
getEnvRead name defval = do
    res <- getEnv name
    case res of
        Nothing -> do
            putStrLn $ "Using default value for " ++ show name
                        ++ " of " ++ show defval
            pure defval
        Just str ->
            case Read.readMaybe str of
                Just val -> pure val
                Nothing  -> die "Error parsing variable value."

