
module Database.PostgreSQL.Simple.Migrate (

    -- * The Migration type
    Mig.Migration,
    Mig.Action(..),

    -- * Creating a migration
    Mig.apply,
    Mig.apply_,
    Mig.delete,
    Mig.noAction,

    -- * Modifying a Migration
    Mig.addPriority,
    Mig.setIsOptional,

    -- * Querying a Migraiton
    Mig.getName,
    Mig.getLocation,
    Mig.getAction,
    Mig.getDependencies,
    Mig.getAddPriority,
    Mig.getIsOptional,

    -- * The SQL Quasiquoter
    --
    -- | We re-export this from PostgreSQL Simple for convience.
    --
    SqlQQ.sql
) where

    import qualified Database.PostgreSQL.Simple.Migrate.Internal.Mig as Mig
    import qualified Database.PostgreSQL.Simple.SqlQQ as SqlQQ

