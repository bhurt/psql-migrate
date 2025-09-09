{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.QQ
-- Description : Comment-omitting SQL QuasiQuoter
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

module Database.PostgreSQL.Simple.Migrate.Internal.QQ (
    mig
) where

    import           Database.PostgreSQL.Simple.SqlQQ (sql)
    import qualified Language.Haskell.TH.Quote        as TH

    -- A comment-emitting SQL quasi-quoter.
    --
    -- This quasi-quoter is just a wrapper around the standard
    -- `Database.PostgreSQL.Simple.SqlQQ.sql` quasi-quoter.  The
    -- extra thing it does is eliminate SQL comments.  This way,
    -- changes to the comments on a migration doesn't change the
    -- fingerprint of the migration, and thus won't cause an error
    -- if the migration has already been applied.
    --
    -- This function will not recognize start of comment inside
    -- \"\" and \'\' style strings.  It will recognize them inside
    -- $x$ style strings.  The assumption here is that these strings
    -- are generally function definitions, which you want to drop
    -- comments in.
    --
    mig :: TH.QuasiQuoter
    mig = TH.QuasiQuoter {
            TH.quoteExp  = TH.quoteExp  sql . s0,
            TH.quotePat  = TH.quotePat  sql . s0,
            TH.quoteType = TH.quoteType sql . s0,
            TH.quoteDec  = TH.quoteDec  sql . s0 }
        where
            -- The default state.  We copy input to output, looking for
            -- either comment starts or string starts.
            s0 :: String -> String
            s0 []             = []
            s0 ('-' : '-'        : xs) = s1 xs
            s0 ('/' : '*'        : xs) = s2 xs
            s0 ('E' : '\''       : xs) = 'E' : '\'' : s4 xs
            s0 ('e' : '\''       : xs) = 'e' : '\'' : s4 xs
            s0 ('U' : '&' : '\'' : xs) = 'U' : '&' : '\'' : s4 xs
            s0 ('u' : '&' : '\'' : xs) = 'u' : '&' : '\'' : s4 xs
            s0 ('\''             : xs) = '\'' : s3 xs
            s0 ('"'              : xs) = '"'  : s5 xs
            s0 ('U' : '&' : '"'  : xs) = 'U' : '&' : '"' : s6 xs
            s0 ('u' : '&' : '"'  : xs) = 'u' : '&' : '"' : s6 xs
            s0 (x                : xs) = x : s0 xs

            -- We are in a single-line -- style comment.  Drop inputs
            -- until we hit the end of line.
            s1 :: String -> String
            s1 []          = []
            s1 ('\n' : xs) = s0 xs
            s1 (_    : xs) = s1 xs

            -- We are in a /* */ style block comment.  Drop inputs
            -- until we see a closing */ sequence.
            s2 :: String -> String
            s2 []               = []
            s2 ('*' : '/' : xs) = s0 xs
            s2 (_         : xs) = s2 xs

            -- We are in a single-quote ' style string.  Copy input to
            -- output, ignoring start of comments or double quoted
            -- strings, until we hit a closing '.  Note that double
            -- single quotes like '' are not closing the string.
            -- Note that this state also handles bit-string style
            -- quotes (B'').  We don't need to intepret or validate
            -- the contents of the string, just copy them.
            s3 :: String -> String
            s3 []                 = []
            s3 ('\'' : '\'' : xs) = '\'' : '\'' : s3 xs
            s3 ('\''        : xs) = '\'' : s0 xs
            s3 (x           : xs) = x    : s3 xs

            -- We are in an escaped single quote string- either E'' or
            -- U&'' style.  This is the same as s3, except a backslash
            -- character escapes the next character.  Note that we don't
            -- need to interpret the string correctly, just copy it out.
            s4 :: String -> String
            s4 []                 = []
            s4 ('\\' : x    : xs) = '\\' : x : s4 xs
            s4 ('\'' : '\'' : xs) = '\'' : '\'' : s3 xs
            s4 ('\''        : xs) = '\'' : s0 xs
            s4 (x           : xs) = x    : s3 xs

            -- Identifier, or double quote "" style string.
            s5 :: String -> String
            s5 []               = []
            s5 ('"' : '"' : xs) = '"' : '"' : s5 xs
            s5 ('"'       : xs) = '"' : s0 xs
            s5 (x         : xs) = x   : s5 xs

            -- Same as s5, except we also recognize the backslash escape.
            s6 :: String -> String
            s6 []               = []
            s6 ('\\' : x  : xs) = '\\' : x : s6 xs
            s6 ('"' : '"' : xs) = '"' : '"' : s6 xs
            s6 ('"'       : xs) = '"' : s0 xs
            s6 (x         : xs) = x   : s6 xs
