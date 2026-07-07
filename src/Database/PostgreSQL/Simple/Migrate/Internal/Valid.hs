
module Database.PostgreSQL.Simple.Migrate.Internal.Valid (
    validate,
    Invalid(..)
) where

    import qualified Control.DeepSeq      as DeepSeq
    import qualified Control.Exception    as Ex
    import           Control.Monad        (when)
    import qualified Control.Monad.Except as Except
    import           Data.Foldable        (traverse_)
    import           Data.Kind            (Type)
    import           Data.Map.Strict      (Map)
    import qualified Data.Map.Strict      as Map
    import           Data.Set             (Set)
    import qualified Data.Set             as Set
    import           Data.Text            (Text)

    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig  as Mig
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Schema 
        as Schema

    data Crumbs = Crumbs {
        steps :: Set Text,
        order :: [ Mig.Migration ]
    }

    type Crumbs :: Type

    type Test a = Either Invalid a

    type Test :: Type -> Type

    validate :: Schema.SchemaState
                    -> [ Mig.Migration ]
                    -> Maybe Invalid
    validate schemaState migList = case tests of
                                Left err -> Just err
                                Right () -> Nothing
        where
            tests :: Test ()
            tests = do
                mp :: Map Text Mig.Migration
                    <- foldr makeMap pure migList Map.empty
                checkCycle mp
                traverse_
                    (checkApp mp)
                    (Schema.getAllApplied schemaState)
                pure ()

            makeMap :: Mig.Migration
                        -> (Map Text Mig.Migration
                            -> Test (Map Text Mig.Migration))
                        -> Map Text Mig.Migration
                        -> Test (Map Text Mig.Migration)
            makeMap mig rest mp =
                case Map.lookup (Mig.getName mig) mp of
                    Nothing   -> rest (Map.insert (Mig.getName mig) mig mp)
                    Just mig2 -> Left $ DuplicateNames mig mig2

            checkApp :: Map Text Mig.Migration -> Text -> Test ()
            checkApp mp aName =
                when (not (Map.member aName mp)) $
                    Left $ UnknownApplied aName

            checkCycle :: Map Text Mig.Migration
                            -> Test ()
            checkCycle mp = do
                    _ <- foldr loop pure mp Set.empty
                    pure ()
                where
                    loop :: Mig.Migration
                            -> (Set Text -> Test (Set Text))
                            -> Set Text
                            -> Test (Set Text)
                    loop mig next memo =
                        callMemo mig startCrumbs memo >>= next

                    startCrumbs :: Crumbs
                    startCrumbs = Crumbs {
                        steps = Set.empty,
                        order = [] }

                    callMemo :: Mig.Migration
                                -> Crumbs
                                -> Set Text
                                -> Test (Set Text)
                    callMemo mig crumbs memo
                        | Set.member (Mig.getName mig) memo = pure memo
                        | otherwise                         =
                            Set.insert (Mig.getName mig) <$>
                                testDeps mig crumbs memo

                    testDeps :: Mig.Migration
                                -> Crumbs
                                -> Set Text
                                -> Test (Set Text)
                    testDeps mig crumbs memo =
                        let newCrumbs :: Crumbs
                            newCrumbs = Crumbs {
                                            steps = Set.insert
                                                        (Mig.getName mig)
                                                        (steps crumbs),
                                            order = mig : order crumbs }
                        in
                        foldr (loop2 mig newCrumbs) pure
                            (Mig.getDependencies mig) memo

                    loop2 :: Mig.Migration
                                -> Crumbs
                                -> Text
                                -> (Set Text -> Test (Set Text))
                                -> Set Text
                                -> Test (Set Text)
                    loop2 mig crumbs depName next memo =
                        getDep mig crumbs depName memo >>= next

                    getDep :: Mig.Migration
                                    -> Crumbs
                                    -> Text
                                    -> Set Text
                                    -> Test (Set Text)
                    getDep mig crumbs depName memo =
                        case Map.lookup depName mp of
                            Nothing  -> unknownDependency mig depName
                            Just dep -> isDelete mig crumbs dep memo

                    isDelete :: Mig.Migration
                                    -> Crumbs
                                    -> Mig.Migration
                                    -> Set Text
                                    -> Test (Set Text)
                    isDelete mig crumbs dep memo =
                        case (Mig.getAction dep) of
                            Mig.Delete -> dependsOnDelete mig dep
                            _          -> isCyclic dep crumbs memo

                    isCyclic :: Mig.Migration
                                -> Crumbs
                                -> Set Text
                                -> Test (Set Text)
                    isCyclic mig crumbs memo
                        | Set.member (Mig.getName mig) (steps crumbs) =
                            cycleDetected mig crumbs
                        | otherwise                         =
                            callMemo mig crumbs memo

                    cycleDetected :: forall a .
                                        Mig.Migration
                                        -> Crumbs
                                        -> Test a
                    cycleDetected root crumbs =
                        Left . CycleDetected root $
                            reverse
                                (takeWhile
                                    (\m -> Mig.getName m /= Mig.getName root)
                                    (order crumbs))

                    unknownDependency :: forall a .
                                            Mig.Migration
                                            -> Text
                                            -> Test a
                    unknownDependency mig depName =
                        Left $ UnknownDependency mig depName

                    dependsOnDelete :: forall a .
                                        Mig.Migration
                                        -> Mig.Migration
                                        -> Test a
                    dependsOnDelete mig dep =
                        Left $ DeleteDependency mig dep

    data Invalid =
        DuplicateNames         Mig.Migration Mig.Migration
        | UnknownDependency    Mig.Migration Text
        | DeleteDependency     Mig.Migration Mig.Migration
        | UnknownApplied       Text
        | CycleDetected        Mig.Migration [ Mig.Migration ]

    type Invalid :: Type

    instance DeepSeq.NFData Invalid where
        rnf (DuplicateNames       x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (UnknownDependency    x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (DeleteDependency     x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (UnknownApplied       x  ) = DeepSeq.rnf x
        rnf (CycleDetected        x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y

    instance Show Invalid where
        show (DuplicateNames m1 m2) =
            "Multiple migrations with the same name: "
                ++ show m1 ++ " and " ++ show m2
        show (UnknownDependency mig dep) =
            "Unknown dependency " ++ show dep ++ " of migration " ++ show mig
        show (DeleteDependency mig dep) =
            "Migraiton " ++ show mig ++ " depends on delete migration "
                ++ show dep
        show (UnknownApplied nm) =
            "Unknown applied migration " ++ show nm
        show (CycleDetected mig migs) =
            "Found cyclic dependencies: "
            ++ "\n    " ++ show mig
            ++ foldMap (\m -> "\n    depends upon " ++ show m) migs
            ++ "\n    depends upon " ++ show mig

    instance Ex.Exception Invalid



