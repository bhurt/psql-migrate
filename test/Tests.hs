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

    import qualified Data.Aeson         as Aeson
    import           Data.List.NonEmpty (NonEmpty (..))
    import           Data.Proxy
    import           Data.Typeable
    import           Test.HUnit
    import qualified Test.QuickCheck    as QuickCheck

    import           Database.PostgreSQL.Simple.Migrate
    import           Database.PostgreSQL.Simple.Migrate.Internal.Order
    import           Database.PostgreSQL.Simple.Migrate.Internal.Error

    -- | orderMigrations unit tests.
    tests :: Test
    tests = TestList [ orderTests, aesonTests ]

    aesonTests :: Test
    aesonTests = TestLabel "aesonTests" $
                    TestList [
                        testJSON (Proxy :: Proxy Replaces),
                        testJSON (Proxy :: Proxy Migration) ]

    quickCheckTest :: QuickCheck.Testable prop => prop -> Test
    quickCheckTest prop = TestCase $ do
                            let args = QuickCheck.stdArgs
                                        { QuickCheck.chatty = False }
                            rval :: QuickCheck.Result
                                <- QuickCheck.quickCheckWithResult args prop
                            assert (QuickCheck.isSuccess rval)

    testJSON :: forall a .
                (Eq a
                , Aeson.ToJSON a
                , Aeson.FromJSON a
                , QuickCheck.Arbitrary a
                , Typeable a
                , Show a)
                => Proxy a
                -> Test
    testJSON proxy = do
            let lbl :: String
                lbl = "JSON encode/decode for " ++ show (typeRep proxy)
            TestLabel lbl $ TestList [
                quickCheckTest propEncode,
                quickCheckTest propToJSON ]
        where
            -- uses toEncoding implicitly
            propEncode :: a -> Bool
            propEncode a = (Aeson.decode' (Aeson.encode a) == Just a)

            -- uses toJSON explicitly
            propToJSON :: a -> Bool
            propToJSON a = Aeson.fromJSON (Aeson.toJSON a) == Aeson.Success a


    orderTests :: Test
    orderTests = TestLabel "orderMigrations" $
                    TestList [
                        testEmptyList,
                        testOneDep,
                        testApplied,
                        unknownApplied,
                        badFingerprintApplied,
                        testInterphase,
                        testDupName,
                        testDupDep,
                        testUnknownDep,
                        testReqOpt,
                        testCircDep1,
                        testCircDep2,
                        testCircDep3,
                        testLaterPhase,
                        testReplace,
                        testNoReplace,
                        testReplace2,
                        testReplaceSelf
                    ]


    -- | Given an empty list, we should return an error.
    testEmptyList :: Test
    testEmptyList = TestLabel "EmptyList" $
                        TestCase $ do
                            assertEqual "" 
                                (orderMigrations [] [])
                                (Left EmptyMigrationList)

    -- If there is a dependency between two migrations, the
    -- depended upon should be returned before the depender.
    testOneDep :: Test
    testOneDep = TestLabel "oneDep" $ TestList [ t1, t2 ]
        where
            t1 :: Test
            t1 = TestLabel "t1" $ TestCase $ do
                    let mig1 :: Migration
                        mig1 = makeMigration "mig-1" "mig 1"
                        mig2 :: Migration
                        mig2 = makeMigration  "mig-2" "mig 2"
                                `addDependency` "mig-1"
                    assertEqual ""
                        (orderMigrations [ mig1, mig2 ] [])
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))

            t2 :: Test
            t2 = TestLabel "t2" $ TestCase $ do
                    let mig1 :: Migration
                        mig1 = makeMigration "mig-1" "mig 1"
                        mig2 :: Migration
                        mig2 = makeMigration  "mig-2" "mig 2"
                                `addDependency` "mig-1"
                    assertEqual ""
                        (orderMigrations [ mig2, mig1 ] [])
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))

    -- | If a migration has already been applied, it should not be in the
    -- returned list of migrations to apply.
    testApplied :: Test
    testApplied =
        TestLabel "applied" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [ ("mig-1", fingerprint mig1) ])
                (Right (Just mig2, [ (Apply, mig2) ]))

    -- | If we've applied a migration that isn't in the list of migrations,
    -- that's an error.
    unknownApplied :: Test
    unknownApplied =
        TestLabel "applied" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [ ("mig-3", fingerprint mig1) ])
                (Left (UnknownMigrations [ "mig-3" ]))

    -- | If a fingerprint of an applied migration doesn't match, that's an
    -- error.
    badFingerprintApplied :: Test
    badFingerprintApplied =
        TestLabel "applied" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [ ("mig-1", fingerprint mig2) ])
                (Left (FingerprintMismatch mig1 (fingerprint mig2)))

    -- | We should be able to depend upon migrations in an earlier phase.
    testInterphase :: Test
    testInterphase =
        TestLabel "interphase" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                        `setPhase` 0
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [])
                (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))

    -- Test detection of duplicate migration names.
    testDupName :: Test
    testDupName =
        TestLabel "dupName" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
            assertEqual "" 
                (orderMigrations [ mig1, mig1 ] [])
                (Left (DuplicateMigrationName mig1 mig1))

    -- | If a dependency is listed twice, that's an error.
    testDupDep :: Test
    testDupDep =
        TestLabel "dupDep" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [])
                (Left (DuplicateDependency mig2 "mig-1"))

    -- | If a dependency isn't in the list, that's an error.
    testUnknownDep :: Test
    testUnknownDep =
        TestLabel "unknownDep" $ TestCase $ do
            let mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig2 ] [])
                (Left (UnknownDependency mig2 "mig-1"))


    -- | A required migration depending on an optional migration is an
    -- error.
    testReqOpt :: Test
    testReqOpt =
        TestLabel "testReqOpt" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                        `setOptional` Optional
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [])
                (Left (RequiredDependsOnOptional mig2 mig1))

    testCircDep1 :: Test
    testCircDep1 =
        TestLabel "circDepSimple" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1 ] [])
                (Left (CircularDependency (mig1 :| [])))

    testCircDep2 :: Test
    testCircDep2 =
        TestLabel "circDepSimple" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                        `addDependency` "mig-2"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [])
                (Left (CircularDependency (mig1 :| [mig2])))

    testCircDep3 :: Test
    testCircDep3 =
        TestLabel "circDepSimple" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                        `addDependency` "mig-3"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
                mig3 :: Migration
                mig3 = makeMigration  "mig-3" "mig 3"
                        `addDependency` "mig-2"
            assertEqual ""
                (orderMigrations [ mig1, mig2, mig3 ] [])
                (Left (CircularDependency (mig1 :| [mig2, mig3])))


    testLaterPhase :: Test
    testLaterPhase =
        TestLabel "laterPhase" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"
                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                        `addDependency` "mig-1"
                        `setPhase` 0
            assertEqual ""
                (orderMigrations [ mig1, mig2 ] [])
                (Left (LaterPhaseDependency mig2 mig1))

    testReplace :: Test
    testReplace =
        TestLabel "replace" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"

                rep1 :: Replaces
                rep1 = makeReplaces "mig-1" (fingerprint mig1)

                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                            `addReplaces` [ rep1 ]
            assertEqual ""
                (orderMigrations [ mig2 ] [ ("mig-1", fingerprint mig1) ])
                (Right (Just mig2, [ (Replace, mig2) ]))

    testNoReplace :: Test
    testNoReplace =
        TestLabel "noReplace" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"

                rep1 :: Replaces
                rep1 = makeReplaces "mig-1" (fingerprint mig1)

                mig2 :: Migration
                mig2 = makeMigration  "mig-2" "mig 2"
                            `addReplaces` [ rep1 ]
            assertEqual ""
                (orderMigrations [ mig2 ] [ ])
                (Right (Just mig2, [ (Apply, mig2) ]))


    testReplace2 :: Test
    testReplace2 =
        TestLabel "replace2" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"

                rep1 :: Replaces
                rep1 = makeReplaces "mig-1" (fingerprint mig1)

                mig2 :: Migration
                mig2 = makeMigration "mig-2" "mig 2"

                rep2 :: Replaces
                rep2 = makeReplaces "mig-2" (fingerprint mig2)

                mig3 :: Migration
                mig3 = makeMigration  "mig-3" "mig 3"
                            `addReplaces` [ rep1, rep2 ]
            assertEqual ""
                (orderMigrations [ mig3 ]
                    [ ("mig-1", fingerprint mig1),
                        ("mig-2", fingerprint mig2) ])
                (Right (Just mig3, [ (Replace, mig3) ]))

    testReplaceSelf :: Test
    testReplaceSelf =
        -- This started life as a bug in testReplace2 above, but exposed
        -- a real bug in the main code.
        TestLabel "replaceSelf" $ TestCase $ do
            let mig1 :: Migration
                mig1 = makeMigration "mig-1" "mig 1"

                rep1 :: Replaces
                rep1 = makeReplaces "mig-1" (fingerprint mig1)

                mig2 :: Migration
                mig2 = makeMigration "mig-2" "mig 2"

                rep2 :: Replaces
                rep2 = makeReplaces "mig-2" (fingerprint mig2)

                mig3 :: Migration
                mig3 = mig2 `addReplaces` [ rep1, rep2 ]
            assertEqual ""
                (orderMigrations [ mig3 ]
                    [ ("mig-1", fingerprint mig1),
                        ("mig-2", fingerprint mig2) ])
                (Right (Just mig3, [ (Replace, mig3) ]))

