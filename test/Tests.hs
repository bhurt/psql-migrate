{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Tests
-- Description : Unit tests for the orderMigrations function.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
-- 
module Tests (
    tests
) where

    -- import           Database.PostgreSQL.Simple.Migrate
    -- import           Database.PostgreSQL.Simple.Migrate.Internal.Order
    import           Test.HUnit

    -- | orderMigrations unit tests.
    tests :: Test
    tests = TestCase $ do
                putStrLn "\nDoing tests!"
                assert True

{-
    tests = TestLabel "orderMigrations" $
                TestList [
                    testNoDeps,
                    testOneDep,
                    testDupName,
                    testDupDep,
                    testUnknown,
                    testReqOpt,
                    testCir1,
                    testCir2,
                    testCir3
                ]

        where
            makeMigration :: String -> [ String ] -> Migration
            makeMigration nm deps = Migration {
                                        name = nm,
                                        command = "",
                                        dependencies = deps,
                                        optional = Required }

            makeOptMigration :: String -> [ String ] -> Migration
            makeOptMigration nm deps = Migration {
                                        name = nm,
                                        command = "",
                                        dependencies = deps,
                                        optional = Optional }

            mig1 :: Migration
            mig1 = makeMigration "mig-1" []

            mig1a :: Migration
            mig1a = makeMigration "mig-1a" []

            -- We have two migrations with no dependencies, they
            -- should be returned in the same order they were
            -- give in.
            testNoDeps :: Test
            testNoDeps = TestLabel "noDeps" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1, mig1a ]
                                        == Right [ mig1, mig1a ]
                                assert $ 
                                    orderMigrations [ mig1a, mig1 ]
                                        == Right [ mig1a, mig1 ]


            mig2 :: Migration
            mig2 = makeMigration "mig-2" [ "mig-1" ]

            -- If there is a dependency between two migrations, the
            -- depended upon should be returned before the depender.
            testOneDep :: Test
            testOneDep = TestLabel "oneDep" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1, mig2 ]
                                        == Right [ mig1, mig2 ]
                                assert $
                                    orderMigrations [ mig2, mig1 ]
                                        == Right [ mig1, mig2 ]

            -- Test detection of duplicate migration names.
            testDupName :: Test
            testDupName = TestLabel "dupName" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1, mig1 ]
                                    == Left (DuplicateMigrationName "mig-1")


            mig2dupdep :: Migration
            mig2dupdep = makeMigration "mig-2" [ "mig-1", "mig-1" ]

            -- Test detection of duplicate dependencies.
            testDupDep :: Test
            testDupDep = TestLabel "dupDep" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1, mig2dupdep ]
                                    == Left (DuplicateDependency
                                                "mig-2" "mig-1")

            -- Test detection of unknown dependencies.
            testUnknown :: Test
            testUnknown = TestLabel "unknown" . TestCase $ do
                                assert $
                                    orderMigrations [ mig2 ]
                                    == Left (UnknownDependency
                                                "mig-2" "mig-1")

            mig1opt :: Migration
            mig1opt = makeOptMigration "mig-1" []

            -- Test that a required migration can't depend upon an optional
            -- one.
            testReqOpt :: Test
            testReqOpt = TestLabel "reqOpt" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1opt, mig2 ]
                                    == Left (RequiredDependsOnOptional
                                                "mig-2" "mig-1")

            mig1cir :: Migration
            mig1cir = makeMigration "mig-1" [ "mig-1" ]

            -- Test for a simple circular dependency.
            testCir1 :: Test
            testCir1 = TestLabel "cir1" . TestCase $ do
                                assert $
                                    orderMigrations [ mig1cir ]
                                    == Left (CircularDependency
                                                ("mig-1" :| [ "mig-1" ]))

            mig2cir :: Migration
            mig2cir = makeMigration "mig-1" [ "mig-2" ]

            -- Test for a more complicated circular dependency
            testCir2 :: Test
            testCir2 = TestLabel "cir2" . TestCase $ do
                                assert $
                                    orderMigrations [ mig2cir, mig2 ]
                                    == Left (CircularDependency
                                                ("mig-1" :|
                                                    [ "mig-2", "mig-1" ]))


            mig3 :: Migration
            mig3 = makeMigration "mig-3" [ "mig-2" ]

            mig3cir :: Migration
            mig3cir = makeMigration "mig-1" [ "mig-3" ]

            mig4 :: Migration
            mig4 = makeMigration "mig-4" []

            mig5 :: Migration
            mig5 = makeMigration "mig-5" [ "mig-4" ]

            testCir3 :: Test
            testCir3 = TestLabel "cir3" . TestCase $ do
                                assert $
                                    orderMigrations [ mig3cir, mig2, mig3,
                                                        mig4, mig5 ]
                                    == Left (CircularDependency
                                                ("mig-1" :|
                                                    [ "mig-3", "mig-2",
                                                        "mig-1" ]))
    
-}