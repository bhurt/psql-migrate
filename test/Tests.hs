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

    import           Data.Maybe                         (isNothing)
    import           Database.PostgreSQL.Simple.Migrate
    import           Test.Hspec

    -- This breaks formatting.
    import Database.PostgreSQL.Simple.Migrate.Internal.Valid

    -- | orderMigrations unit tests.
    tests :: Spec
    tests = describe "Valid" $ do
            it "succeeds on an empty list." $
                isNothing $ validate defaultState []
            it "succeeds on a simple schema." $
                let s :: Schema
                    s = noAction "foo" []
                in
                isNothing $ validate defaultState (s defaultState)
            it "succeeds with a dependency" $ 
                let s :: Schema
                    s = noAction "foo" []
                        <> noAction "bar" [ "foo" ]
                in
                isNothing $ validate defaultState (s defaultState)
            it "detects a non-existant dependency" $
                let s :: Schema
                    s = noAction "foo" [ "bar" ]
                in
                (EqInvalid <$> validate defaultState (s defaultState))
                    == Just (EqInvalid (UnknownDependency (getMig s) "bar"))
                
            it "detects duplicate names" $
                let s1 :: Schema
                    s1 = noAction "foo" [ ]

                    s2 :: Schema
                    s2 = noAction "foo" []
                in
                (EqInvalid <$> validate defaultState ((s1 <> s2) defaultState))
                    == Just (EqInvalid
                                (DuplicateNames (getMig s2) (getMig s1)))

            it "detects a self-dependency" $
                let s1 :: Schema
                    s1 = noAction "foo" [ "foo" ]
                in
                (EqInvalid <$> validate defaultState (s1 defaultState))
                    == Just (EqInvalid (CycleDetected (getMig s1) []))

            it "detects a two-step cycle" $
                let s1 :: Schema
                    s1 = noAction "foo" [ "bar" ]

                    s2 :: Schema
                    s2 = noAction "bar" [ "foo" ]
                in
                (EqInvalid <$> validate defaultState ((s1 <> s2) defaultState))
                    == Just (EqInvalid (CycleDetected (getMig s2)
                                            [ getMig s1 ]))
            it "detects a tthree-step cycle" $
                let s1 :: Schema
                    s1 = noAction "foo" [ "bar" ]

                    s2 :: Schema
                    s2 = noAction "bar" [ "baz" ]

                    s3 :: Schema
                    s3 = noAction "baz" [ "foo" ]

                    res :: Maybe Invalid
                    res = validate defaultState
                                ((s1 <> s2 <> s3) defaultState)
                in (EqInvalid <$> res)
                        == Just (EqInvalid (CycleDetected (getMig s2)
                                            [ getMig s3, getMig s1 ]))


    defaultState :: SchemaState
    defaultState = SchemaState {
                        getAllApplied = mempty,
                        isUpgrading   = True }

    getMig :: Schema -> Migration
    getMig schema = case schema defaultState of
                        x : _ -> x
                        _     -> error "No migration!"


    eqMig :: Migration -> Migration -> Bool
    eqMig x y = (getName x == getName y)
                && (getLocation x == getLocation y)

    eqInvalid :: Invalid -> Invalid -> Bool
    eqInvalid (DuplicateNames x1 x2) (DuplicateNames y1 y2) =
        eqMig x1 y1 && eqMig x2 y2
    eqInvalid (UnknownDependency x1 x2) (UnknownDependency y1 y2) =
        eqMig x1 y1 && x2 == y2
    eqInvalid (UnknownApplied x) (UnknownApplied y) = x == y
    eqInvalid (CycleDetected x xs) (CycleDetected y ys) =
        let loop :: [ Migration ] -> [ Migration ] -> Bool
            loop [] [] = True
            loop [] _  = False
            loop _  [] = False
            loop (p : ps) (q : qs) = eqMig p q && loop ps qs
        in
        eqMig x y && loop xs ys
    eqInvalid _ _ = False

    newtype EqInvalid = EqInvalid { getInvalid :: Invalid }

    instance Eq EqInvalid where
        x == y = eqInvalid (getInvalid x) (getInvalid y)
