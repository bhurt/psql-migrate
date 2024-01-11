
-- |
-- Module      : Database.PostgreSQL.Simple.Migrate
-- Description : Yet another migration tool for Haskell/PostgreSQL projects.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- = Justification
--
-- Given that there are already several very good SQL schema management
-- tools (such as Liquibase) and several Haskell libraries for doing
-- schema management, why write another one?
--
-- The answer is that psql-migrate has a different philosophy for
-- maintaining the schema.  Rather than being a separate tool with
-- separate data from the main program, it's designed to integrate
-- into the code.  Migrations are just another data structure,
-- intendend to live right along side the other data structures and
-- functions that depend on them.
--
-- This change in philosophy gives rise to four unusual features:
-- dependencies, phases, optional, and replaces.
--
-- = Introduction
--
-- == Migrations
--
-- The core structure to this module is the `Migration`.  A migration is
-- just a structure with two required and several optional members.  The
-- two required members are the name of the migration and the command.
--
-- The name of the migration is just a string, which uniquely identifies
-- the migration.  The name should be unique in any set of changes.  It
-- is used to both test whether the migration has been applied or not,
-- and to express dependencies between migrations.  The command is just
-- the SQL to be execute to apply the migration to a database, and is
-- a `Database.PostgreSQL.Simple.Query`.  We recommend using the
-- `Database.PostgreSQL.Simple.SqlQQ.sql` quasiquoter for convience.
--
-- To make declaring migrations simple, we supply the `makeMigration`
-- function.  This takes a name and command to execute, making a
-- simple migration just:
--
-- @
--     makeMigration "example-1"
--        [sql| CREATE TABLE foo(
--                 bar INT PRIMARY KEY); |]
-- @
--
-- There are several optional fields, which can be set with various
-- update functions.  So, for example, a more complicated migration
-- might be:
--
-- @
--     makeMigration "example-2"
--         [sql| ALTER TABLE foo ADD COLUMN baz VARCHAR; |]
--         \`addDependency\` "example-1"
--         \`setPhase\` 2
-- @
--
-- Don't worry about what they do at the moment- the important point is
-- that `addDependency` and `setPhase` are just functions that modify
-- the migration created by `makeMigration`.
--
-- == Schema in Code
--
-- The huge advantage of migrations being just plain Haskell structures
-- is that the migrations can live in the source code, next to the
-- functions and data structures which use that schema.  For example,
-- we might have a user module defined like:
--
-- @
-- module User (User(..), getUser, migrations) where
--
--     data User = User {
--                  userId   :: Int,
--                  userName :: String }
--
--     getUser :: Connection -> Int -> IO User
--     getUser = ...
-- 
--     migrations :: [ Migration ]
--     migration = [
--          makeMigration "user-1"
--              [sql| CREATE TABLE users(
--                      id SERIAL PRIMARY KEY,
--                      name VARCHAR NOT NULL); |]
--          ]
-- @
--
-- Keeping the schema next to the code that uses it makes it significantly
-- easier to keep both in sync.
--
-- == Applying Migrations
--
-- TODO: write this
--
-- = Nifty Features
--
-- == Explicit Dependencies
--
-- This library supplies an `apply` function, which takes a database
-- connection (or at least a way to construct one) and a list of
-- migrations.  It then applies those migrations which have not already
-- been applied to the database schema.  It can be used to create a
-- migration app:
--
-- @
-- module ApplyApp (
--    main
-- ) where
--
--      import qualified Database.PostgreSQL.Simple as PG
--      import Database.PostgreSQL.Simple.Migrate
--      import qualified Messages
--      import qualified User
--
--      main :: IO ()
--      main = do
--          _ <- apply Medium allMigrations
--                  (PG.connect PG.defaultConnectionOptions)
--          pure ()
--
--      allMigrations :: [ Migration ]
--      allMigrations = []
--          -- Sorted in alphabetical order,
--          -- because why not?
--          ++ Messages.migrations
--          ++ User.migrations
-- @
--
-- The `apply` function checks whether all the migrations in the given
-- list have been applied, and applies the ones that haven't.  But this
-- raises the interesting question: in what order are the changes
-- applied?  It is very common for one migration to depend upon another.
-- For example, we might want to add a password column to our users table.
-- But adding a column to a table that hasn't yet been created doesn't
-- make sense.
--
-- The solution to this is to use explicit dependencies.  The
-- `addDependency` function is used to add a dependency:
-- 
-- @
-- module User (User(..), getUser, migrations) where
--
--     ...
--
--     migrations :: [ Migration ]
--     migration = [
--          makeMigration "user-1"
--              [sql| CREATE TABLE users(
--                      id SERIAL PRIMARY KEY,
--                      name VARCHAR NOT NULL); |],
--          makeMigration "user-2"
--              [sql| ALTER TABLE users
--                      ADD COLUMN password VARCHAR; |]
--              \`addDependency\` "user-1"
--          ]
-- @
-- 
-- Here, the user-2 migration explicitly depends upon the user-1 migration-
-- and `apply` will ensure that user-1 is applied first.
--
-- == Phases
--
-- Dependencies are the primary way to order migrations.  But some
-- times, some migrations just want to happen before or after everything
-- else.  For example, you might want to create a dbtype table, labelling
-- the database as production, test, or development:
--
-- @
--  migrations :: [ Migration ]
--  migrations = [
--      makeMigration "dbtype-1"
--          [sql| CREATE TABLE dbtype(
--                  only_row BOOL PRIMARY KEY
--                              DEFAULT TRUE,
--                  dbtype TEXT NOT NULL,
--                  CONSTRAINT only_one_row
--                      CHECK (only_row); |]
--          \`setPhase\` 0,
--      makeMigration "db-type-insert-1"
--          [sql| INSERT INTO dbtype(dbtype)
--                  VALUES(\'development\'); |]
--          \`addDependency\` "dbtype-1"
--          \`setPhase\` 0
--      ]
-- @
--
-- We want these migrations to run first, before everything else.  We
-- could just add a dependency to \"db-type-insert-1\" to every other
-- constraint, or we could use phases.
--
-- Every migration has a phase.  By default, `makeMigration` sets this
-- phase to 1, which is generally correct.  The rule is: every migration
-- of phase N happens before any migration of phase N+1 happens.  So
-- by setting these migrations to be phase 0, they will happen \"first\".
--
-- == Optional
--
-- There is a chicken or the egg problem in applying new migrations
-- to a database that is actively being used.  Do you update the program
-- that is using the database first, or apply the schema changes first? 
-- Updating the program first can cause problems as it tries to use
-- schema changes that haven't been applied yet.  But applying the schema
-- first has the same problem- the program is now not aware of the
-- schema changes.
--
-- The solution is to be able to mark certain migrations as optional.
-- This means they may or may not have been applied (yet).  We can then
-- write our server to test (if needed) whether or not the optional
-- migration has been applied.  The application can test whether
-- the migration has been applied or not with the query:
--
-- @
--  SELECT EXISTS (
--      SELECT 1
--      FROM schema_migrations
--      WHERE name = 'name'
--      LIMIT 1);
-- @
--
-- Note that the name will be lower-case, as migration names are
-- case-insensitive.
--
-- The server can then do one thing if the migration has not been applied,
-- and another thing once it has.  This solves the chicken and the egg
-- problem.  You deploy the server first.  It then continually queries the
-- database for whether the migration has been applied yet or not.  As
-- it hasn't been applied, the application does the old thing.  Then,
-- some time later, you apply the migration.  Migrations are applied
-- in a transaction, and schema updates in PostgreSQL are transactional
-- (one of the many, many reasons I love PostgreSQL), either the
-- entire migration has been applied, or none of it has.  Once it's
-- been applied, the application immediately starts doing the new thing.
--
-- Much later, the migration can be changed to no longer be optional.
-- At which point, newer versions of the application can be deployed
-- which omit both the test for the optional migration, and the old
-- way of doing things.
--
-- == Replaces
--
-- The \"code bloat\" of having the migrations in the code, in the
-- executable, is considered to be minor.  Any database-accessing
-- program is probably well into the tens of megabytes of executable
-- size even without the migrations being included- an additional
-- 10KB or 100KB of migration data isn't a big deal.  Text is small.
-- Heck, the entire corpus of \"War and Peace\" comes in at only
-- about 3MB (in unformatted text).
--
-- Never the less, a complex migration history makes it hard to
-- understand what the current schema is.  This is where replaces
-- comes in.  A migration can be marked that it replaces one or
-- more other migrations.
--
-- Say you have a schema with multiple modifications, like:
--
-- @
--  migrations :: [ Migration ]
--  migrations = [
--      makeMigration "users-1"
--          [sql| CREATE TABLE users (
--                      id SERIAL PRIMARY KEY,
--                      name TEXT NOT NULL); |],
--      makeMigration "users-2"
--          [sql| ALTER TABLE users
--                  ADD COLUMN password TEXT; |]
--          \`addDependency\` "users-1",
--      makeMigration "users-3"
--          [sql| ALTER TABLE users
--                  ADD COLUMN email TEXT; |]
--          \`addDependency\` "users-2",
--      makeMigration "users-4"
--          [sql| ALTER TABLE users
--                  ALTER COLUMN password SET NOT NULL; |]
--          \`addDependency\` "users-3"
--      ]
-- @
--
-- This is a pretty simple example, compared to real-world cases.  But
-- even here, it's somewhat difficult to keep track of what the users
-- table current schema.  What we can do is replace the whole lift of
-- migrations with a single migration that replaces all of them:
--
-- @
--  migrations :: [ Migrations ]
--  migrations = [
--      makeMigration "users-5"
--          [sql| CREATE TABLE users(
--                  id SERIAL PRIMARY KEY,
--                  name TEXT NOT NULL,
--                  password TEXT NOT NULL,
--                  email TEXT); |]
--          \`addReplaces\` [
--              makeReplaces "users-1"
--                  "9wbC-yXz6ISXeA-eFOn-l4DVoJZ4P8I79HJxJKHIBIg=",
--              makeReplaces "users-2"
--                  "t_isz1eBAu8XM_e2idYdnCPvI-UFEpFC3s2RTyjArDA=",
--              makeReplaces "users-3"
--                  "wL4RJs17fjpSEvogt4RdoLk1LvfzfAZ2IVabbuwKOcE=",
--              makeReplaces "users-4"
--                  "F8_nmgURLqBjyQfKEOZAL4ugZhZxYyGPqPA3Rbtk3sI=" 
--          ]
--      ]
-- @
--
-- This says that our new \"users-5\" migration replaces the old
-- \"users-1\" through \"users-4\" migrations.
--
-- The \"9wbC-blahblahblah\" stuff is the fingerprint of the migration
-- (the base-64 encoding of the SHA3-256 hash of the command).  This
-- can be obtained either by calling the `makeFingerprint` command,
-- or better yet getting the fingerprint out of the database.
--
module Database.PostgreSQL.Simple.Migrate (
    -- * Migration type
    Migration(..),
    Optional(..),
    Replaces(..),

    -- * Creating migrations
    makeMigration,
    makeReplaces,

    -- ** Migration modifiers
    addDependency,
    addDependencies,
    setOptional,
    setPhase,
    addReplaces,

    -- ** Replaces modifiers
    setReplacesOptional,

    -- * Applying migrations
    apply,
    check,
    Verbose(..),

    -- * Checking for Optional migrations
    migrationIsApplied,
    appliedOptionalMigrations,

    -- * Calculating fingerprints
    --
    -- | Useful for constructing `Replaces` structures.
    --
    makeFingerprint
) where

    import Database.PostgreSQL.Simple.Migrate.Internal.Apply
    import Database.PostgreSQL.Simple.Migrate.Internal.Finger
    import Database.PostgreSQL.Simple.Migrate.Internal.Opt
    import Database.PostgreSQL.Simple.Migrate.Internal.Types
