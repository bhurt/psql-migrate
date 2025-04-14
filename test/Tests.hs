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
    import           Data.List.NonEmpty (NonEmpty ((:|)))
    import           Data.Typeable
    import qualified Test.QuickCheck    as QuickCheck
    import           Test.Syd

    import           Database.PostgreSQL.Simple.Migrate
    import           Database.PostgreSQL.Simple.Migrate.Internal.Order
    import           Database.PostgreSQL.Simple.Migrate.Internal.Error

    -- | orderMigrations unit tests.
    tests :: Spec
    tests = do
        orderTests
        aesonTests

    aesonTests :: Spec
    aesonTests = do
        jsonTest (Proxy :: Proxy Replaces)
        jsonTest (Proxy :: Proxy Migration)

    jsonTest :: forall a .
                    (QuickCheck.Arbitrary a
                    , Show a
                    , Eq a
                    , Typeable a
                    , Aeson.ToJSON a
                    , Aeson.FromJSON a)
                    => Proxy a 
                    -> Spec
    jsonTest proxy = do
        describe ("JSON roundtrip tests for " ++ show (typeRep proxy)) $ do
            it "parseJSON . toJSON is identity" $ 
                let go :: a -> Bool
                    go a = Aeson.fromJSON (Aeson.toJSON a) == Aeson.Success a
                in
                QuickCheck.property go
            it "decode . encode is identity" $
                let go :: a -> Bool
                    go a = Aeson.decode' (Aeson.encode a) == Just a
                in
                QuickCheck.property go

    orderTests :: Spec
    orderTests = 
        describe "Testing the order function." $ do

            it "detects an empty migration list" $
                orderMigrations [] [] `shouldBe` Left EmptyMigrationList

            it "orders dependencies correctly" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in do

                orderMigrations [ mig1, mig2 ] []
                    `shouldBe` 
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))
                orderMigrations [ mig2, mig1 ] []
                    `shouldBe` 
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))


            it "doesn't return applied migrations" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig1, mig2 ] [ ("mig-1", fingerprint mig1) ]
                    `shouldBe` (Right (Just mig2, [ (Apply, mig2) ]))

            it "detects unknown migration" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"

                    mig3 :: Migration
                    mig3 = makeMigration  "mig-3" "mig 3"
                in
                orderMigrations [ mig1, mig2 ]
                    [ ("mig-3", fingerprint mig3) ]
                    `shouldBe` (Left (UnknownMigrations [ "mig-3" ]))

            it "detects fingerprint mismatches" $ 
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"

                    mig3 :: Migration
                    mig3 = makeMigration  "mig-3" "mig 3"
                in
                orderMigrations [ mig1, mig2 ] [ ("mig-1", fingerprint mig3) ]
                    `shouldBe`
                        (Left (FingerprintMismatch mig1 (fingerprint mig3)))

            it "handles phases correctly" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                            `setPhase` 0

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                in do
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe`
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))
                orderMigrations [ mig2, mig1 ] []
                    `shouldBe`
                        (Right (Just mig1, [ (Apply,  mig1), (Apply, mig2) ]))

            it "should detect duplicate names" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration "mig-1" "mig-2"
                in
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe` (Left (DuplicateMigrationName mig1 mig2))

            it "should detect duplicate dependencies" $ 
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe` (Left (DuplicateDependency mig2 "mig-1"))

            it "detects missing dependencies" $
                let mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig2 ] []
                    `shouldBe` (Left (UnknownDependency mig2 "mig-1"))

            it "detects a required migration to depend upon \
                \ an optional one" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                            `setOptional` Optional

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe` (Left (RequiredDependsOnOptional mig2 mig1))
                
            it "detects a migration depending upon itself" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                                `addDependency` "mig-1"
                in
                orderMigrations [ mig1 ] []
                    `shouldBe`
                        (Left (CircularDependency (mig1 :| [])))

            it "detects a 2-step circular dependency" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                            `addDependency` "mig-2"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe`
                        (Left (CircularDependency (mig1 :| [ mig2])))
                
            it "detects a 3-step circular dependency" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                            `addDependency` "mig-3"

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"

                    mig3 :: Migration
                    mig3 = makeMigration "mig-3" "mig-3"
                            `addDependency` "mig-2"
                in
                orderMigrations [ mig1, mig2, mig3 ] []
                    `shouldBe`
                        (Left (CircularDependency (mig1 :| [ mig2, mig3])))

            it "detects a migration depending upon one in a later phase" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"
                            `setPhase` 2

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addDependency` "mig-1"
                in
                orderMigrations [ mig1, mig2 ] []
                    `shouldBe` (Left (LaterPhaseDependency mig2 mig1))

            it "handles replacements correctly" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    rep1 :: Replaces
                    rep1 = makeReplaces "mig-1" (fingerprint mig1)

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addReplaces` [ rep1 ]
                in
                orderMigrations [ mig2 ] [ ("mig-1", fingerprint mig1) ]
                    `shouldBe` (Right (Just mig2, [ (Replace, mig2) ]))

            it "applies migrations if what they're replacing isn't there" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    rep1 :: Replaces
                    rep1 = makeReplaces "mig-1" (fingerprint mig1)

                    mig2 :: Migration
                    mig2 = makeMigration  "mig-2" "mig 2"
                            `addReplaces` [ rep1 ]
                in
                orderMigrations [ mig2 ] [ ]
                    `shouldBe` (Right (Just mig2, [ (Apply, mig2) ]))

            it "handles replacing multiple migrations" $
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
                in
                orderMigrations [ mig3 ]
                    [ ("mig-1", fingerprint mig1),
                        ("mig-2", fingerprint mig2) ]
                    `shouldBe`
                        (Right (Just mig3, [ (Replace, mig3) ]))

            -- This started life as a bug in testReplace2 above, but exposed
            -- a real bug in the main code.
            it "detects a migration replacing itself" $
                let mig1 :: Migration
                    mig1 = makeMigration "mig-1" "mig 1"

                    rep1 :: Replaces
                    rep1 = makeReplaces "mig-1" (fingerprint mig1)

                    mig1a :: Migration
                    mig1a = mig1 `addReplaces` [ rep1 ]
                in
                orderMigrations [ mig1a ] []
                    `shouldBe` (Left (SelfReplacement mig1a))
