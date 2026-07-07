{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.Migrate.Internal.Mig (
    -- * The Schema type
    Schema,

    -- * The Migration type
    Migration,
    Action(..),

    -- * Creating a migration
    apply,
    apply_,
    delete,
    noAction,

    -- * Querying a Migration
    getName,
    getAction,
    getDependencies,
    getLocation,

    -- * Modifying a Migration
    setDependencies,
    setAction

) where

    import qualified Control.DeepSeq                    as DeepSeq
    import           Data.Kind                          (Type)
    import           Data.Text                          (Text)
    import qualified Database.PostgreSQL.Simple         as PG
    import qualified Database.PostgreSQL.Simple.ToField as PG
    import qualified Database.PostgreSQL.Simple.ToRow   as PG
    import qualified Database.PostgreSQL.Simple.Types   as PG
    import qualified GHC.Stack                          as Stack

    -- This breaks formatting.
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Schema
        as Schema

    type Schema =  Schema.SchemaState -> [ Migration ]

    data Action =
        Apply PG.Query [ PG.Action ]
        | Delete
        | NoAction

    type Action :: Type

    data Migration = Migration {
        getName :: !Text,
        getLocation :: String,
        getAction :: Action,
        getDependencies :: [ Text ] }

    type Migration :: Type

    getLoc :: Stack.CallStack -> String
    getLoc stack =
        case Stack.getCallStack stack of
            []           -> "" -- This should never happen
            (loc, _) : _ -> loc

    apply :: forall row .
                (PG.ToRow row
                , Stack.HasCallStack)
                => Text
                -> PG.Query
                -> row
                -> [ Text ]
                -> Schema
    apply getName query row getDependencies =
        let getAction :: Action
            getAction = Apply query (PG.toRow row)
            getLocation :: String
            getLocation = getLoc Stack.callStack
        in
        \_ -> [ Migration { .. } ]

    apply_ :: Stack.HasCallStack
                => Text
                -> PG.Query
                -> [ Text ]
                -> Schema
    apply_ getName query getDependencies =
        let getAction :: Action
            getAction = Apply query []
            getLocation :: String
            getLocation = getLoc Stack.callStack
        in
        \_ -> [ Migration { .. } ]

    delete :: Stack.HasCallStack
                => Text
                -> [ Text ]
                -> Schema
    delete getName getDependencies =
        let getAction :: Action
            getAction = Delete
            getLocation :: String
            getLocation = getLoc Stack.callStack
        in
        \_ -> [ Migration { .. } ]

    noAction :: Stack.HasCallStack
                => Text
                -> [ Text ]
                -> Schema
    noAction getName getDependencies =
        let getAction :: Action
            getAction = NoAction
            getLocation :: String
            getLocation = getLoc Stack.callStack
        in
        \_ -> [ Migration { .. } ]


    setDependencies :: [ Text ] -> Migration -> Migration
    setDependencies deps mig = mig { getDependencies = deps }

    setAction :: Action -> Migration -> Migration
    setAction act mig = mig { getAction = act }

    newtype ForceAction = ForceAction PG.Action

    type ForceAction :: Type

    instance DeepSeq.NFData ForceAction where
        rnf (ForceAction act) = 
            case act of
                PG.Plain bldr           -> DeepSeq.rwhnf bldr
                PG.Escape bst           -> DeepSeq.rnf bst
                PG.EscapeByteA bst      -> DeepSeq.rnf bst
                PG.EscapeIdentifier bst -> DeepSeq.rnf bst
                PG.Many acts            -> DeepSeq.rnf (ForceAction <$> acts)

    instance DeepSeq.NFData Action where
        rnf (Apply q acts) = 
            DeepSeq.rnf (PG.fromQuery q)
            `seq` DeepSeq.rnf (ForceAction <$> acts)
        rnf Delete         = ()
        rnf NoAction       = ()

    instance DeepSeq.NFData Migration where
        rnf mig =
            DeepSeq.rnf (getName mig)
            `seq` DeepSeq.rnf (getAction mig)
            `seq` DeepSeq.rnf (getDependencies mig)

    instance Show Migration where
        show mig = show (getName mig)
                    ++ (case getLocation mig of
                            ""  -> ""
                            loc -> "(at " ++ loc ++ ")")

