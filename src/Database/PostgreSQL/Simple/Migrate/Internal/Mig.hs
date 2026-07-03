{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.Migrate.Internal.Mig (
    -- * The Migration type
    Migration,
    Action(..),

    -- * Creating a migration
    apply,
    apply_,
    delete,
    noAction,

    -- * Modifying a Migration
    addPriority,
    setIsOptional,

    -- * Querying a Migraiton
    getName,
    getLocation,
    getAction,
    getDependencies,
    getAddPriority,
    getIsOptional

) where

    import qualified Control.DeepSeq                    as DeepSeq
    import           Data.Kind                          (Type)
    import           Data.Text                          (Text)
    import qualified Database.PostgreSQL.Simple         as PG
    import qualified Database.PostgreSQL.Simple.ToField as PG
    import qualified Database.PostgreSQL.Simple.ToRow   as PG
    import qualified Database.PostgreSQL.Simple.Types   as PG
    import qualified GHC.Stack                          as Stack

    data Action =
        Apply PG.Query [ PG.Action ]
        | Delete
        | NoAction

    type Action :: Type

    data Migration = Migration {
        getName :: !Text,
        getLocation :: String,
        getAction :: Action,
        getDependencies :: [ Text ],
        getAddPriority :: {-# UNPACK #-} !Word,
        getIsOptional :: {-# UNPACK #-} !Bool }

    type Migration :: Type

    instance Eq Migration where
        m1 == m2 = compare m1 m2 == EQ

    instance Ord Migration where
        compare m1 m2 = compare (getName m1) (getName m2)

    makeMig :: Text
                -> Stack.CallStack
                -> Action
                -> [ Text ]
                -> Migration
    makeMig getName cs getAction getDependencies =
        let getLocation :: String
            getLocation = 
                case Stack.getCallStack cs of
                    (loc, _) : _ -> loc
                    []           -> ""

            getAddPriority :: Word
            getAddPriority = 0

            getIsOptional :: Bool
            getIsOptional = False
        in
        Migration { .. }


    apply :: forall row .
                (PG.ToRow row
                , Stack.HasCallStack)
                => Text
                -> PG.Query
                -> row
                -> [ Text ]
                -> Migration
    apply getName query row =
        makeMig
            getName
            Stack.callStack
            (Apply query (PG.toRow row))

    apply_ :: Stack.HasCallStack
                => Text
                -> PG.Query
                -> [ Text ]
                -> Migration
    apply_ getName query =
        makeMig
            getName
            Stack.callStack
            (Apply query [])

    delete :: Stack.HasCallStack
                => Text
                -> [ Text ]
                -> Migration
    delete getName =
        makeMig
            getName
            Stack.callStack
            Delete

    noAction :: Stack.HasCallStack
                => Text
                -> [ Text ]
                -> Migration
    noAction getName =
        makeMig
            getName
            Stack.callStack
            NoAction

    addPriority :: Migration -> Word -> Migration
    addPriority mig p = mig { getAddPriority = p + getAddPriority mig }

    setIsOptional :: Migration -> () -> Migration
    setIsOptional mig () = mig { getIsOptional = True }

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
            `seq` DeepSeq.rnf (getLocation mig)
            `seq` DeepSeq.rnf (getAction mig)
            `seq` DeepSeq.rnf (getDependencies mig)
            `seq` DeepSeq.rnf (getAddPriority mig)
            `seq` DeepSeq.rnf (getIsOptional mig)

    instance Show Migration where
        show mig = show (getName mig)
                        ++ (if getLocation mig == ""
                            then ""
                            else "(at " ++ (getLocation mig) ++ ")")

