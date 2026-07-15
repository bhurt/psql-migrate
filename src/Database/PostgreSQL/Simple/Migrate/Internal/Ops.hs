
module Database.PostgreSQL.Simple.Migrate.Internal.Ops (
    optional,
    shuffle
) where

    import           Data.Text        (Text)
    import qualified System.IO.Unsafe
    import qualified System.Random
    import           System.Random    (StdGen)

    -- This breaks formatting
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig as Mig

    optional :: Mig.Schema -> Mig.Schema
    optional schema schemaState =
        if Mig.isUpgrading schemaState
        then schema schemaState
        else filter (Mig.isApplied schemaState) $ schema schemaState

    shuffle :: Mig.Schema -> Mig.Schema
    shuffle schema schemaState =
            -- The use of unsafePerformIO here is morally correct.  We don't
            -- case when it happens.
            System.IO.Unsafe.unsafePerformIO $
                System.Random.getStdRandom go
        where
            go :: StdGen -> ([Mig.Migration], StdGen)
            go gen1 =
                let migs1 :: [ Mig.Migration ]
                    migs1 = schema schemaState

                    migs2 :: [ Mig.Migration ]
                    gen2 :: StdGen
                    (migs2, gen2)  = System.Random.uniformShuffleList
                                        migs1 gen1
                in
                foldr loop (\g -> ([], g)) migs2 gen2

            loop :: Mig.Migration
                    -> (StdGen -> ([Mig.Migration], StdGen))
                        -> StdGen
                        -> ([Mig.Migration], StdGen)
            loop mig rest gen1 =
                let gen2 :: StdGen
                    deps :: [ Text ]
                    (deps, gen2) = System.Random.uniformShuffleList
                                    (Mig.getDependencies mig) gen1

                    mig2 :: Mig.Migration
                    mig2 = Mig.setDependencies deps mig

                    gen3 :: StdGen
                    migs :: [ Mig.Migration ]
                    (migs, gen3) = rest gen2
                in
                (mig2 : migs, gen3)

