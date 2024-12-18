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
    -- import           Data.List.NonEmpty (NonEmpty (..))
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
                        testOneDep
                        -- testDupName,
                        -- testDupDep,
                        -- testUnknown,
                        -- testReqOpt,
                        -- testCir1,
                        -- testCir2,
                        -- testCir3
                    ]


    testEmptyList :: Test
    testEmptyList = TestLabel "EmptyList" $
                        TestCase $ do
                            assert (orderMigrations [] []
                                        == Left EmptyMigrationList)

    mig1 :: Migration
    mig1 = makeMigration "mig-1" "mig 1"

    mig2 :: Migration
    mig2 = makeMigration "mig-2" "mig 2" 

    -- If there is a dependency between two migrations, the
    -- depended upon should be returned before the depender.
    testOneDep :: Test
    testOneDep = TestLabel "oneDep" . TestCase $ do
                        let mig2a :: Migration
                            mig2a = mig2 `addDependency` "mig-1"
                        assert $
                            orderMigrations [ mig1, mig2a ] []
                                == Right (Just mig1,
                                            [ (Apply,  mig1), (Apply, mig2) ])
                        assert $
                            orderMigrations [ mig2a, mig1 ] []
                                == Right (Just mig1,
                                            [ (Apply,  mig1), (Apply, mig2) ])

    {-
    -- Test detection of duplicate migration names.
    testDupName :: Test
    testDupName = TestLabel "dupName" . TestCase $ do
                        assert $
                            checkAndOrder [ mig1, mig1 ]
                            == Left (DuplicateMigrationName "mig-1")


    mig2dupdep :: Migration
    mig2dupdep = makeMigration "mig-2" "" 
                    `addDependencies` [ "mig-1", "mig-1" ]

    -- Test detection of duplicate dependencies.
    testDupDep :: Test
    testDupDep = TestLabel "dupDep" . TestCase $ do
                        assert $
                            checkAndOrder [ mig1, mig2dupdep ]
                            == Left (DuplicateDependency
                                        "mig-2" "mig-1")

    -- Test detection of unknown dependencies.
    testUnknown :: Test
    testUnknown = TestLabel "unknown" . TestCase $ do
                        assert $
                            checkAndOrder [ mig2 ]
                            == Left (UnknownDependency
                                        "mig-2" "mig-1")

    mig1opt :: Migration
    mig1opt = makeMigration "mig-1" ""
                    `setOptional` Optional

    -- Test that a required migration can't depend upon an optional
    -- one.
    testReqOpt :: Test
    testReqOpt = TestLabel "reqOpt" . TestCase $ do
                        assert $
                            checkAndOrder [ mig1opt, mig2 ]
                            == Left (RequiredDependsOnOptional
                                        "mig-2" "mig-1")

    mig1cir :: Migration
    mig1cir = makeMigration "mig-1" "" `addDependency` "mig-1"

    -- Test for a simple circular dependency.
    testCir1 :: Test
    testCir1 = TestLabel "cir1" . TestCase $ do
                        assert $
                            checkAndOrder [ mig1cir ]
                            == Left (CircularDependency
                                        ("mig-1" :| [ "mig-1" ]))

    mig2cir :: Migration
    mig2cir = makeMigration "mig-1" "" `addDependency` "mig-2"

    -- Test for a more complicated circular dependency
    testCir2 :: Test
    testCir2 = TestLabel "cir2" . TestCase $ do
                    assert $
                        checkAndOrder [mig2cir, mig2 ]
                            == Left (CircularDependency
                                        ("mig-1" :| [ "mig-2" ]))


    mig3 :: Migration
    mig3 = makeMigration "mig-3" "" `addDependency` "mig-2"

    mig3cir :: Migration
    mig3cir = makeMigration "mig-1" "" `addDependency` "mig-3"

    mig4 :: Migration
    mig4 = makeMigration "mig-4" ""

    mig5 :: Migration
    mig5 = makeMigration "mig-5" "" `addDependency` "mig-4"

    testCir3 :: Test
    testCir3 = TestLabel "cir3" . TestCase $ do
                        assert $
                            checkAndOrder [ mig3cir, mig2, mig3,
                                                mig4, mig5 ]
                            == Left (CircularDependency
                                        ("mig-1" :|
                                            [ "mig-2", "mig-3" ]))
    -}
