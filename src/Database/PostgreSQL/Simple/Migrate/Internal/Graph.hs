
module Database.PostgreSQL.Simple.Migrate.Internal.Graph (
    Graph,
    getGraph,
    makeGraph,
    GraphError(..)
) where

    import qualified Control.DeepSeq      as DeepSeq
    import           Control.Monad        (when)
    import qualified Control.Monad.Except as Except
    import qualified Control.Monad.Reader as Reader
    import           Data.Foldable        (traverse_)
    import           Data.Kind            (Type)
    import           Data.Map.Strict      (Map)
    import qualified Data.Map.Strict      as Map
    import           Data.Set             (Set)
    import qualified Data.Set             as Set
    import           Data.Text            (Text)

    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Memo as Memo
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig  as Mig

    data Graph = MkGraph { getGraph :: Map Text Mig.Migration }

    type Graph :: Type

    type Test = Except.Except GraphError

    type Test :: Type -> Type

    data Crumbs = Crumbs {
        steps :: Set Text,
        order :: [ Mig.Migration ]
    }

    type Crumbs :: Type

    type Cycle = Memo.MemoT Mig.Migration () (Reader.ReaderT Crumbs Test)

    type Cycle :: Type -> Type

    makeGraph :: Map Text Bool
                    -> [ Mig.Migration ]
                    -> Bool
                    -> Either GraphError Graph
    makeGraph applied migList strict = Except.runExcept tests
        where
            tests :: Test Graph
            tests = do
                mp :: Map Text Mig.Migration
                    <- foldr makeMap pure migList Map.empty
                checkMigs mp
                traverse_ (checkApp mp) (Map.keys applied)
                pure $ MkGraph mp

            makeMap :: Mig.Migration
                        -> (Map Text Mig.Migration
                            -> Test (Map Text Mig.Migration))
                        -> Map Text Mig.Migration
                        -> Test (Map Text Mig.Migration)
            makeMap mig rest mp =
                case Map.lookup (Mig.getName mig) mp of
                    Nothing   -> rest (Map.insert (Mig.getName mig) mig mp)
                    Just mig2 -> Except.throwError $
                                    DuplicateNames mig mig2

            checkApp :: Map Text Mig.Migration -> Text -> Test ()
            checkApp mp aName =
                when (not (Map.member aName mp)) $
                    Except.throwError $ UnknownApplied aName

            checkMigs :: Map Text Mig.Migration -> Test ()
            checkMigs mp = runCycle $ traverse_ Memo.getMemo mp

                where

                    runCycle :: Cycle () -> Test ()
                    runCycle act =
                        let act2 :: Reader.ReaderT Crumbs Test ()
                            act2 = Memo.runMemoT checkMig act

                        in
                        Reader.runReaderT act2 emptyCrumbs

                    emptyCrumbs :: Crumbs
                    emptyCrumbs = Crumbs {
                                    steps = Set.empty,
                                    order = [] }

                    checkMig :: Mig.Migration -> Cycle ()
                    checkMig  mig = do
                            checkCycle
                            traverse_ checkDep (Mig.getDependencies mig)
                            when (strict
                                    && (not (Mig.getIsOptional mig))
                                    && (not (Map.member
                                                (Mig.getName mig)
                                                applied))) $
                                Except.throwError $ RequiredNotApplied mig
                        where
                            checkCycle :: Cycle ()
                            checkCycle = do
                                crumbs :: Crumbs <- Reader.ask
                                when (Set.member (Mig.getName mig)
                                        (steps crumbs)) $
                                    Except.throwError $
                                        FoundCycle mig
                                            (reverse
                                                (takeWhile
                                                    (\m ->
                                                        Mig.getName m
                                                        /= Mig.getName mig)
                                                    (order crumbs)))

                            checkDep :: Text -> Cycle ()
                            checkDep depName =
                                case Map.lookup depName mp of
                                    Nothing -> Except.throwError $
                                                    UnknownDependency
                                                        mig depName
                                    Just dep -> do
                                        when (isDelete dep) $
                                            Except.throwError $
                                                DeleteDependency mig dep
                                        when (not (Mig.getIsOptional mig)
                                                && (Mig.getIsOptional dep)) $
                                            Except.throwError $
                                                OptionalDependency mig dep
                                        when (Map.member (Mig.getName mig)
                                                    applied
                                                && not (Map.member
                                                            (Mig.getName dep)
                                                            applied))  $
                                            Except.throwError $
                                                DependencyNotApplied mig dep
                                        Reader.local addStep $
                                            Memo.getMemo dep

                            isDelete :: Mig.Migration -> Bool
                            isDelete mg = case Mig.getAction mg of
                                                Mig.Delete   -> True
                                                Mig.Apply {} -> False
                                                Mig.NoAction -> False

                            addStep :: Crumbs -> Crumbs
                            addStep crumbs = Crumbs {
                                steps = Set.insert (Mig.getName mig)
                                                        (steps crumbs),
                                order = mig : order crumbs }


    data GraphError =
        DuplicateNames         Mig.Migration Mig.Migration
        | UnknownDependency    Mig.Migration Text
        | DeleteDependency     Mig.Migration Mig.Migration
        | OptionalDependency   Mig.Migration Mig.Migration
        | DependencyNotApplied Mig.Migration Mig.Migration
        | UnknownApplied       Text
        | RequiredNotApplied   Mig.Migration
        | FoundCycle           Mig.Migration [ Mig.Migration ]

    type GraphError :: Type

    instance DeepSeq.NFData GraphError where
        rnf (DuplicateNames       x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (UnknownDependency    x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (DeleteDependency     x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (OptionalDependency   x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (DependencyNotApplied x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y
        rnf (UnknownApplied       x  ) = DeepSeq.rnf x
        rnf (RequiredNotApplied   x  ) = DeepSeq.rnf x
        rnf (FoundCycle           x y) = DeepSeq.rnf x `seq` DeepSeq.rnf y

    instance Show GraphError where
        show (DuplicateNames m1 m2) =
            "Multiple migrations with the same name: "
                ++ show m1 ++ " and " ++ show m2
        show (UnknownDependency mig dep) =
            "Unknown dependency " ++ show dep ++ " of migration " ++ show mig
        show (DeleteDependency mig dep) =
            "Migraiton " ++ show mig ++ " depends on delete migration "
                ++ show dep
        show (OptionalDependency mig dep) =
            "Required migration " ++ show mig
                ++ " depends on optional migration " ++ show dep
        show (DependencyNotApplied mig dep) =
            "Migration " ++ show mig ++ " applied, but dependency "
                ++ show dep ++ " is not applied"
        show (UnknownApplied nm) =
            "Unknown applied migration " ++ show nm
        show (RequiredNotApplied mig) =
            "Required migration " ++ show mig ++ " not applied"
        show (FoundCycle mig migs) =
            "Found cyclic dependencies: "
            ++ "\n    " ++ show mig
            ++ foldMap (\m -> "\n    depends upon " ++ show m) migs
            ++ "\n    depends upon " ++ show mig




