{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Order
-- Description : Order migrations
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of "Database.PostgreSQL.Simple.Migrate",
-- you probably want that module instead.  Anything exported by this
-- module that is not also exported by the main module is subject
-- to change without notice.
--
-- This module contains the things we can do without querying the
-- database- including basic sanity checks and ordering of the
-- migrations.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Order (
    Apply(..),
    orderMigrations
) where

    import           Control.Monad        (unless, when)
    import           Data.CaseInsensitive (CI)
    import qualified Data.CaseInsensitive as CI
    import qualified Data.Foldable        as Foldable
    import qualified Data.Graph           as Graph
    import           Data.IntMap.Strict   (IntMap)
    import qualified Data.IntMap.Strict   as IntMap
    import           Data.Kind            (Type)
    import qualified Data.List            as List
    import           Data.List.NonEmpty   (NonEmpty ((:|)))
    import           Data.Map.Strict      (Map)
    import qualified Data.Map.Strict      as Map
    import           Data.Maybe           (mapMaybe)
    import           Data.Set             (Set)
    import qualified Data.Set             as Set
    import           Data.Text            (Text)
    import qualified GHC.Generics         as Generics

    -- These screw up the formatting, so they get their own,
    -- unformatted, block.
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Error as Error
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Types as Types

    -- | They type of a node in the graph.
    --
    -- Using a structure instead of a tuple for correctness reasons.
    data Node = Node {
                    getMigration :: Types.Migration,
                    getKey :: CI Text,
                    getEdges :: [ CI Text ] }

    type Node :: Type

    -- | A graph and associated operations.
    --
    -- Using a structure instead of a tuple for correctness reasons.
    --
    -- We'd like to call this a Graph, but that name is taken.  If I
    -- think of a better name, I'll rename it.
    --
    data Grph = Grph {
                -- | The graph object
                getGraph :: Graph.Graph,

                -- | Look up a node given it's vertex.
                getNode :: Graph.Vertex -> Node,

                -- | Look up a vertext, given it's key.
                --
                -- Returns Nothing if the key is not in the graph.
                getVertex :: CI Text -> Maybe Graph.Vertex }

    type Grph :: Type

    -- | Boolean analog for whether we need to apply or replace a mgiration.
    --
    -- We're already checking whether the replaced migrations exist in
    -- the database or not, just remember which one it is.
    data Apply = Apply | Replace
        deriving stock (Show, Read, Ord, Eq, Enum, Bounded, Generics.Generic)

    type Apply :: Type

    -- | Order the migrations
    orderMigrations ::
        [ Types.Migration ]
        -- ^ The list of migrations
        -> [ (Text, Text) ]
        -- ^ The pre-existing migrations (name, fingerprint)
        -> Either Error.MigrationsError
            (Maybe Types.Migration, [ (Apply, Types.Migration) ])
    orderMigrations []   _       = Left Error.EmptyMigrationList
    orderMigrations migs applied = do -- Either monad

        -- Make the name -> migration map.
        migMap :: Map (CI Text) Types.Migration
            <- Foldable.foldlM makeMigMap Map.empty migs

        -- Make the name -> fingerprint map from the migrations
        -- already applied to the database.
        fpMap :: Map (CI Text) Text
            <- Foldable.foldlM makeFingerprintMap Map.empty applied

        -- Check all the migrations for validity.
        --
        -- remMap: Remaining map, those migrations that were in
        -- the fpMap but not accounted for in the migrations list.
        -- If this map isn't empty, that's an error.
        --
        -- apMap: The map of migrations that have not been applied
        -- to the database, and whether they are an Apply (a migration
        -- that needs to be applied) or Replace (a migration that
        -- is simply replacing an existing set of migrations).
        --
        ((remMap, apMap), ()) 
            :: ((Map (CI Text) Text, Map (CI Text) Apply), ())
            <- runStateM
                    (Foldable.traverse_ (checkMigrations migMap) migs)
                    (fpMap, Map.empty)

        -- Check that remMap is empty- if not, it's an error.
        case (Map.keys remMap) of
            []     -> pure ()
            (m:ms) -> Left
                        . Error.UnknownMigrations
                        . fmap CI.original
                        $ (m :| ms)

        -- Create the per-phase graphs
        let zgrphs :: IntMap Grph
            zgrphs = makeGraphs migMap

        -- Check for cycles
        Foldable.traverse_ cycleCheck zgrphs

        -- Note that these two function calls are explicitly left as
        -- lazy thunks- only one will be forced.
        pure (requiredUnapplied migMap apMap, sortMigs zgrphs apMap)


    -- | Add a migration to an accumulating map of names to migrations.
    --
    -- Note that we only de-dup here, we don't do deeper checks.
    --
    makeMigMap :: Map (CI Text) Types.Migration
                    -> Types.Migration
                    -> Either Error.MigrationsError
                        (Map (CI Text) Types.Migration)
    makeMigMap migMap mig =
        case Map.lookup (Types.name mig) migMap of
            Nothing -> pure $ Map.insert (Types.name mig) mig migMap
            Just mig2 -> Left $ Error.DuplicateMigrationName mig mig2

    -- | Add a migration name and fingerprint to an accumulating map.
    --
    -- Note that we only dedup here, we don't do deeper checks.
    --
    makeFingerprintMap :: Map (CI Text) Text
                            -> (Text, Text)
                            -> Either Error.MigrationsError (Map (CI Text) Text)
    makeFingerprintMap apMap (oName, fp) =
        let nm :: CI Text
            nm = CI.mk oName
        in
        case Map.lookup nm apMap of
            Nothing -> pure $ Map.insert nm fp apMap
            Just _  -> Left $ Error.DuplicateExisting oName

    -- | Check a migration for validity.
    --
    -- This is where most of the sanity checking happens.
    --
    checkMigrations :: Map (CI Text) Types.Migration
                        -> Types.Migration
                        -> StateM ()
    checkMigrations migMap mig = do
            liftEither baseChecks
            mfp :: Maybe Text <- getFingerprint (Types.name mig)
            case mfp of
                Nothing -> do
                    applyType :: Apply <- getApplyType (Types.replaces mig)
                    setApply (Types.name mig) applyType
                Just fp
                    | fp /= Types.fingerprint mig ->
                        failM $ Error.FingerprintMismatch mig fp
                    | otherwise             ->
                        Foldable.traverse_ noReplaces (Types.replaces mig)
        where

            -- Do all the basic checking.
            --
            -- Checks that don't depend upon what migrations have been
            -- applied already.
            baseChecks :: Either Error.MigrationsError ()
            baseChecks = do
                _ <- Foldable.foldlM checkDupe Set.empty
                        (Types.dependencies mig)
                deps :: [ Types.Migration ]
                    <- traverse lookupDep (Types.dependencies mig)
                when (Types.optional mig == Types.Required) $
                    Foldable.traverse_ checkNoOpt deps
                Foldable.traverse_ checkPhase deps
                Foldable.traverse_ checkCirc (Types.dependencies mig)
                case Types.replaces mig of
                    [] -> pure ()
                    xs -> baseCheckReplaces xs

            -- Check that we don't have duplicate dependencies.
            -- checks on it.
            checkDupe :: Set (CI Text)
                            -> CI Text
                            -> Either Error.MigrationsError (Set (CI Text))
            checkDupe st nm =
                if Set.member nm st
                    then Left $ Error.DuplicateDependency mig (CI.original nm)
                    else pure $ Set.insert nm st

            -- Convert from dependency name to migration structure.
            --
            -- Errors out if the migration doesn't exist.
            lookupDep :: CI Text
                            -> Either Error.MigrationsError Types.Migration
            lookupDep nm =
                case Map.lookup nm migMap of
                    Nothing  ->
                        Left $ Error.UnknownDependency mig (CI.original nm)
                    Just dep -> pure dep


            -- Check that we don't depend upon on optional migration.
            --
            -- This is only called when we are a required migration.
            checkNoOpt :: Types.Migration -> Either Error.MigrationsError ()
            checkNoOpt dep =
                when (Types.optional dep == Types.Optional) $
                    Left $ Error.RequiredDependsOnOptional mig dep

            -- Check that we are not in an early phase than a dependency.
            checkPhase :: Types.Migration -> Either Error.MigrationsError ()
            checkPhase dep =
                when (Types.phase mig < Types.phase dep) $
                    Left $ Error.LaterPhaseDependency mig dep

            -- Check that we're not a trivial circular dependency
            -- (i.e. we depend on ourselves)
            checkCirc :: CI Text -> Either Error.MigrationsError ()
            checkCirc depName =
                when (depName == Types.name mig) $
                    Left . Error.CircularDependency $ mig :| [ ]

            -- Do basic checking of replaces.
            --
            -- Two checks: no replaced migration should be in the list
            -- of migrations (i.e. in the migMap).  And there should be
            -- at least one required replacement.
            baseCheckReplaces :: [ Types.Replaces ]
                                    -> Either Error.MigrationsError ()
            baseCheckReplaces [] = pure ()
            baseCheckReplaces repls = do
                Foldable.traverse_ checkForSelfReplacement repls
                checkForRequiredReplacement repls
                _ <- Foldable.foldlM checkDupeReplacement Set.empty repls
                Foldable.traverse_ checkReplacesDoesNotExist repls

            -- Check that we're not trying to replace ourselves.
            checkForSelfReplacement :: Types.Replaces
                                        -> Either Error.MigrationsError ()
            checkForSelfReplacement repl =
                when (Types.rName repl == Types.name mig) $
                    Left (Error.SelfReplacement mig)

            -- Check that we have at least one required replacement.
            -- Note that we don't have to deal with the empty list,
            -- as that has already been handled.
            checkForRequiredReplacement :: [ Types.Replaces ]
                                            -> Either Error.MigrationsError ()
            checkForRequiredReplacement repls =
                if any (\r -> Types.rOptional r == Types.Required) repls
                then pure ()
                else Left $ Error.NoRequiredReplacement mig

            -- Check that we do not have duplicate replacements
            checkDupeReplacement ::
                Set (CI Text)
                -> Types.Replaces
                -> Either Error.MigrationsError (Set (CI Text))
            checkDupeReplacement st rep =
                if Set.member (Types.rName rep) st
                then Left $
                        Error.DuplicateReplaces mig (CI.original
                                                    (Types.rName rep))
                else pure $ Set.insert (Types.rName rep) st

            -- Check that a replacement does not exist.
            checkReplacesDoesNotExist :: Types.Replaces
                                            -> Either Error.MigrationsError ()
            checkReplacesDoesNotExist rep =
                case Map.lookup (Types.rName rep) migMap of
                    Nothing   -> pure ()
                    Just rmig -> Left $ Error.ReplacedStillInList mig rmig

            -- | We should not have any of the replaced migrations in
            -- the database.
            --
            -- This is called when the migration that is replacing them
            -- has already been applied to the database.  So the
            -- migrations it is replacing should have been removed
            -- already.
            noReplaces :: Types.Replaces -> StateM ()
            noReplaces repl = do
                mfp :: Maybe Text <- getFingerprint (Types.rName repl)
                case mfp of
                    Nothing -> pure ()
                    Just _  -> failM $ Error.ReplacedStillInDB mig
                                            (CI.original (Types.rName repl))

            -- | Make sure that either all the required replaced migrations
            -- are in the database, or no replaced migrations are in the
            -- database.
            getApplyType :: [ Types.Replaces ] -> StateM Apply
            getApplyType repls = do
                -- Run through all the replacements, and see if they're
                -- in the database.  Check their fingerprints if they
                -- are.
                dbs :: [ (Bool, Types.Replaces) ] <- traverse isInDB repls
                if any fst dbs
                then
                    -- At least one replaced migration is in the database.
                    -- We don't care if it's optional or required.  But
                    -- this means that every required replacement should
                    -- be in the database.  It's an error if this isn't
                    -- so.
                    let f :: (Bool, Types.Replaces) -> Bool
                        f (inDB, repl) =
                            not inDB
                            && (Types.rOptional repl == Types.Required)
                    in
                    case List.find f dbs of
                        Nothing  -> pure Replace
                        Just (_, repl) ->
                            failM $ Error.RequiredReplacementMissing mig
                                            (CI.original (Types.rName repl))
                else pure Apply

            isInDB :: Types.Replaces -> StateM (Bool, Types.Replaces)
            isInDB repl = do
                mfp :: Maybe Text <- getFingerprint (Types.rName repl)
                case mfp of
                    Nothing -> pure (False, repl)
                    Just fp ->
                        if fp == Types.rFingerprint repl
                        then pure (True, repl)
                        else failM $ Error.ReplacedFingerprint mig
                                            (CI.original (Types.rName repl))

    -- | Turn a MigMap into a set of graphs, one per phase.
    makeGraphs :: Map (CI Text) Types.Migration -> IntMap Grph
    makeGraphs migMap = makeG <$> fullG
        where
            makeG :: Map (CI Text) Node -> Grph
            makeG nmap = toGrph . Graph.graphFromEdges $
                            fromNode <$> Map.elems nmap

            -- The full graph
            fullG :: IntMap (Map (CI Text) Node)
            fullG = Map.foldl' go initG migMap
                where
                    go :: IntMap (Map (CI Text) Node)
                            -> Types.Migration
                            -> IntMap (Map (CI Text) Node)
                    go zmap mig =
                        -- adjust is the correct function to use
                        -- here- every phase with a migration
                        -- should be a member of zmap.
                        IntMap.adjust (addEdges mig)
                            (Types.phase mig)
                            zmap

                    addEdges :: Types.Migration
                                -> Map (CI Text) Node
                                -> Map (CI Text) Node
                    addEdges mig nmap =
                        List.foldl'
                            (addEdge (Types.name mig))
                            nmap
                            (Types.dependencies mig)

                    addEdge :: CI Text
                                -> Map (CI Text) Node
                                -> CI Text
                                -> Map (CI Text) Node
                    addEdge target nmap source =
                        -- adjust is the correct function to use
                        -- here- if the source is in a different
                        -- phase than the target, we don't want
                        -- to add the edge (the phases take care
                        -- of enforcing the ordering).
                        Map.adjust (plusEdge target) source nmap

                    plusEdge :: CI Text -> Node -> Node
                    plusEdge target node =
                        node { getEdges = target : getEdges node }

            -- Our initial graph, where we've added all the nodes,
            -- but no edges.
            initG :: IntMap (Map (CI Text) Node)
            initG = Map.foldl' go IntMap.empty migMap
                where
                    go :: IntMap (Map (CI Text) Node) -> Types.Migration ->
                            IntMap (Map (CI Text) Node)
                    go zmap mig =
                        let node :: Node
                            node = Node {
                                    getMigration = mig,
                                    getKey = Types.name mig,
                                    getEdges = [] }
                        in
                        IntMap.alter (addNode node) (Types.phase mig) zmap

                    addNode :: Node
                                -> Maybe (Map (CI Text) Node)
                                -> Maybe (Map (CI Text) Node)
                    addNode node Nothing =
                        Just $ Map.singleton (getKey node) node
                    addNode node (Just nmap) =
                        Just $ Map.insert (getKey node) node nmap

    -- | Check a Grph for cycles.
    --
    -- Note: we only check for intra-phase cycles, cycles within a single
    -- phase.  Intra-phase cycles, cycles that involve multiple different
    -- phases, will, by necessity, include dependencies from migrations
    -- in later phases to migrations in earlier phases.  Which is an error
    -- caught earlier in the process.
    --
    cycleCheck :: Grph -> Either Error.MigrationsError ()
    cycleCheck grph =
            let trees :: Graph.Forest Graph.Vertex
                trees = Graph.scc (getGraph grph)
            in
            Foldable.traverse_ checkTree trees
        where
            checkTree :: Graph.Tree Graph.Vertex
                            -> Either Error.MigrationsError ()
            checkTree tree =
                case Foldable.toList tree of
                    []     -> pure ()
                    -- If we only have one element in this strongly
                    -- connected component, that is not an error.
                    -- The only way this can be a circular dependency
                    -- is if it's a trivial circular dependency (i.e.
                    -- a migration that directly relies on itself)- and
                    -- we've already checked for that back in checkCirc,
                    -- when we were doing the base sanity checks.
                    [_]    -> pure ()
                    (x:xs) -> Left . Error.CircularDependency $
                                    fixup <$> (x :| xs)

            fixup :: Graph.Vertex -> Types.Migration
            fixup vtx = 
                let node :: Node
                    node = getNode grph vtx
                in
                getMigration node

    -- | Convert a tuple to a Node structure.
    toNode :: (Types.Migration, CI Text, [ CI Text ]) -> Node
    toNode (m, k, es) = Node {  getMigration = m,
                                getKey = k,
                                getEdges = es }

    -- | Convert a Node structure back into a tuple.
    fromNode :: Node -> (Types.Migration, CI Text, [ CI Text ])
    fromNode node = (getMigration node, getKey node, getEdges node)


    -- | Convert a tuple to a Grph structure.
    toGrph :: (Graph.Graph,
                    Graph.Vertex -> (Types.Migration, CI Text, [ CI Text ]),
                    CI Text -> Maybe Graph.Vertex)
                -> Grph
    toGrph (graph, getn, getv) =
        Grph {
            getGraph = graph,
            getNode = toNode <$> getn,
            getVertex = getv }

    -- | Do the topological sort of the graphs, and filter for
    -- only those that need applying or replacing.
    --
    sortMigs  :: IntMap Grph
                    -> Map (CI Text) Apply
                    -> [ (Apply, Types.Migration) ]
    sortMigs zgrphs apMap =
        let grs1 :: [ (Int, Grph) ]
            grs1 = IntMap.toAscList zgrphs

            grs2 :: [ Grph ]
            grs2 = snd <$> grs1

            grs3 :: [ [ Types.Migration ] ]
            grs3 = tsort <$> grs2

            grs4 :: [ Types.Migration ]
            grs4 = concat grs3

            f :: Types.Migration -> Maybe (Apply, Types.Migration)
            f mig =
                case Map.lookup (Types.name mig) apMap of
                    Nothing -> Nothing
                    Just ap -> Just (ap, mig)
        in
        if Map.null apMap
        then []
        else mapMaybe f grs4

    -- | Do a topological sort of a graph.
    tsort :: Grph -> [ Types.Migration ]
    tsort grph =
        let vtxs :: [ Graph.Vertex ]
            vtxs = Graph.topSort (getGraph grph)

            nodes :: [ Node ]
            nodes = getNode grph <$> vtxs

        in
        getMigration <$> nodes

    -- | Are there any migrations that are required, yet not applied?
    requiredUnapplied :: Map (CI Text) Types.Migration
                            -> Map (CI Text) Apply
                            -> Maybe Types.Migration
    requiredUnapplied migMap apmap =
        let f :: CI Text -> Maybe Types.Migration -> Maybe Types.Migration
            f nm rest =
                case Map.lookup nm migMap of
                    -- This should never happen.
                    Nothing                                    -> rest
                    Just mig 
                        | Types.optional mig == Types.Required -> Just mig
                        | otherwise                            -> rest
        in
        foldr f Nothing (Map.keys apmap)


    -- | A state monad.
    --
    -- I don't want to depend upon MTL just for one bit of code, but
    -- having a transformer stack in one place is _really useful_.
    -- So I just hand-roll my own.
    --
    newtype StateM a =
        StateM {
            runStateM ::
                (Map (CI Text) Text, Map (CI Text) Apply)
                -> Either
                    Error.MigrationsError
                    ((Map (CI Text) Text, Map (CI Text) Apply), a) }

    type StateM :: Type -> Type
    type role StateM representational

    instance Functor StateM where
        fmap f sm = StateM $ fmap (fmap f) . runStateM sm

    instance Applicative StateM where
        pure a = StateM $ \s -> Right (s, a)
        sm1 <*> sm2 =
            StateM $ \s0 -> do -- Either monad
                (s1, f) <- runStateM sm1 s0
                (s2, a) <- runStateM sm2 s1
                Right (s2, f a)

    instance Monad StateM where
        return = pure
        xm >>= f =
            StateM $ \s0 -> do -- Either Monad
                (s1, x) <- runStateM xm s0
                runStateM (f x) s1

    -- | Look up a fingerprint
    --
    -- If the migation name existing in the map of name/fingerprints,
    -- then remove it from that map, and return Just the fingerprint.
    -- Otherwise, return Nothing.
    getFingerprint :: CI Text -> StateM (Maybe Text)
    getFingerprint nm = StateM $
        \(fpm, apm) ->
            case Map.lookup nm fpm of
                Nothing -> Right ((fpm, apm), Nothing)
                Just fp -> Right ((Map.delete nm fpm, apm), Just fp)

    -- | Set the apply value.
    setApply :: CI Text -> Apply -> StateM ()
    setApply nm ap =
        StateM $ \(fpm, apm) -> Right ((fpm, Map.insert nm ap apm), ())

    -- | Lift an either into a StateM
    liftEither :: Either Error.MigrationsError a -> StateM a
    liftEither e = StateM $ \s -> (s,) <$> e

    -- | Fail with a migrations error.
    failM :: Error.MigrationsError -> StateM a
    failM err = StateM $ \_ -> Left err
