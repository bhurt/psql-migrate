
module Database.PostgreSQL.Simple.Migrate (
    -- * The Schema type
    Mig.Schema,
    Mig.SchemaState(..),

    -- * The Migration type
    Mig.Migration,
    Mig.Action(..),

    -- * Creating a migration
    Mig.apply,
    Mig.apply_,
    Mig.delete,
    Mig.noAction,

    -- * Querying a Migration
    Mig.getName,
    Mig.getAction,
    Mig.getDependencies,
    Mig.getLocation,

    -- * Modifying a Migration
    Mig.setDependencies,
    Mig.setAction,

    -- * SchemaState accessors
    Mig.isApplied,
    Mig.isInitializing,

    -- * Schema operations
    Ops.optional,
    Ops.shuffle,

    -- * SQL Quasi-Quoter
    --
    -- | Re-exported from PostgreSQL.Simple for convience.
    PG.sql
) where

    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig
        as Mig
    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Ops
        as Ops
    import qualified Database.PostgreSQL.Simple.SqlQQ as PG

