{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
    mig,
    migi
) where

    import qualified Data.Char                  as Char
    import           Data.Maybe                 (fromMaybe)
    import           Data.String                (fromString)
    import qualified Data.Text                  as Text
    import qualified Database.PostgreSQL.Simple as PG
    import qualified Language.Haskell.TH        as TH
    import qualified Language.Haskell.TH.Quote  as TH

    -- | A comment-emitting, space compressing SQL quasi-quoter.
    --
    -- This quasi-quoter is similar to the
    -- `Data.PostgreSQL.Simple.SqlQQ.sql` quoter, in that it
    -- reduces multiple spaces (or other characters that
    -- `Data.Char.isSpace` returns True for) by a single space.
    --
    -- In addition, it converts comments, both the line-ending
    -- style (--) and the multi-line style (/* ... */) into
    -- single spaces, which can then be combined with other
    -- spaces into single spaces.
    --
    -- A word on strings is needed.  Inside a single-quote
    -- string (\'whatever\', including Postgresql-special
    -- quotes like U\'whatever\' or E\'whatever\'), and inside
    -- identifier quotes (\"whatever\"), multiple spaces are
    -- not compressed to single spaces, comments are not recognized,
    -- and backslash characters always escape the next character.
    -- The contents are copied verbatim to the output stream.
    --
    -- THis is not done for $ quotes ($key$whatever$key$).  The
    -- assumption here is that dollar quotes are used for things
    -- like function bodies, which need compressing, while single
    -- quote strings are used for data that should not be compressed.
    -- If these assumptions are not right for your use case, use a
    -- different quasiquoter.
    --
    mig :: TH.QuasiQuoter
    mig = TH.QuasiQuoter {
            TH.quoteExp  = pure . toQuery . stringE . compress,
            TH.quotePat  = err,
            TH.quoteType = err,
            TH.quoteDec  = err }
        where
            err :: forall a . a
            err = error "mig quasiqupter can only be used as an expression."

    -- | A comment-emitting, space compressing, and string interpolating
    --  SQL quasi-quoter.
    --
    -- This quasi-quoter converts comments into spaces and compresses
    -- multiple spaces into single spaces, like the `mig` quasiquoter
    -- does, so all the comments about it applies here as well.
    --
    -- Then, in a different pass, this quasiquoter does variable 
    -- interpolation.  This pass recognizes interpolated variables of
    -- the form ${validHaskellName} - where validHaskellName is a
    -- lowercase or underscore character, followed by zero or more
    -- alphanumeric or underscore characters.  No spaces are allowed
    -- in the interpolation.  Nor are apostrophes ('), as they confuse
    -- the compress code.
    --
    -- It is assumed that the name given is a valid haskell variable
    -- in scope at the site of the quasiqouter of type `Data.Text.Text`.
    -- It is then interpolated into the SQL value being produced. 
    --
    -- The idea here is that the list of migrations we're producing
    -- doesn't have to be static.  It is now possible to write
    -- parameterized migration lists, like:
    --
    -- @
    --  createIdIndex :: Text -> Migration
    --  createIdIndex tableName =
    --      makeMigration (tableName <> "-index-id-1")
    --          [migi|
    --              CREATE INDEX ON ${tableName}(id);
    --          |]
    -- @
    --
    -- This allows for sharing of common miggrations, in this case against
    -- multiple different tables.
    --
    -- Interpolation happends *after* compression- so the strings
    -- interpolated do not have comments replaced with spaces, or multiple
    -- spaces reduced to single spaces.  Nor are the values further
    -- interpolated.  This means interpolation can be used to include
    -- problematic SQL subsequences, like:
    --
    -- @
    --  problemMigration :: Migration
    --  problemMigration = makeMigration "problem" [migi|
    --      UPDATE TABLE example SET field = ${badsql}; |]
    --      where
    --          badsql :: Text
    --          badsql = "$X$ This is literally included.  $X$"
    -- @
    --
    -- Note that the interpolator only includes simple haskell names, not
    -- more complicated expressions.  It is always legal to provide local
    -- let or where clauses that calculate the expressions.  The GHC
    -- optimizer will then inline these definitions where necessary.
    --
    migi :: TH.QuasiQuoter
    migi = TH.QuasiQuoter {
            TH.quoteExp  = pure . toQuery . interpolate . compress,
            TH.quotePat  = err,
            TH.quoteType = err,
            TH.quoteDec  = err }
        where
            err :: forall a . a
            err = error "migi quasiqupter can only be used as an expression."

    toQuery :: TH.Exp -> TH.Exp
    toQuery s = TH.SigE (TH.AppE (TH.VarE 'fromString) s)
                (TH.ConT ''PG.Query)

    stringE :: String -> TH.Exp
    stringE = TH.LitE . TH.StringL

    compress :: String -> String
    compress = skipSpaces . copyMain False
                    -- We do an ectra skipSpaces here to skip any
                    -- initial spaces.  Note that doing it in reverse, like:
                    --      copyMain False . skipSpaces
                    -- doesn't work, if the string starts with a comment.
                    -- So we either have to horribly complicate skipSpaces,
                    -- or just do it after the copyMain.  copyMain should
                    -- never emit more than 1 leading space, so it isn't
                    -- a major cost to do it after.
        where

            -- The main loop.
            --
            -- Note that we avoid emitting a space until we know we need
            -- to.  That's the Bool passed in.  
            copyMain :: Bool -> String -> String
            -- Note: even if we are "owed" a space, we don't output it
            -- if we're at the end of the string- no trailing whitespace.
            copyMain _  []               = []
            copyMain _  ('-' : '-' : xs) = copyMain True (skipLC xs)
            copyMain _  ('/' : '*' : xs) = copyMain True (skipMC xs)
            copyMain ns ('\''      : xs) = preSpace ns ('\'' : copyQuote xs)
            copyMain ns ('"'       : xs) = preSpace ns ('"' : copyIdentifier xs)
            copyMain ns (x         : xs)
                | Char.isSpace x         = copyMain True (skipSpaces xs)
                | otherwise              = preSpace ns (x : copyMain False xs)

            -- Prepend a space if we need it.
            preSpace :: Bool -> String -> String
            preSpace True  = (' ' :)
            preSpace False = id

            -- Skip isSpace characters
            skipSpaces :: String -> String
            skipSpaces [] = []
            skipSpaces (x : xs)
                | Char.isSpace x = skipSpaces xs
                | otherwise      = (x : xs)

            -- Skip line comments (--)
            skipLC :: String -> String
            skipLC []          = []
            skipLC ('\n' : xs) = xs
            skipLC (_    : xs)    = skipLC xs

            -- Skip multi-line comments (/* ... */)
            skipMC :: String -> String
            skipMC [] = []
            skipMC ('*' : '/' : xs) = xs
            skipMC (_ : xs) = xs

            -- Copy quotes literally
            copyQuote :: String -> String
            copyQuote []                 = []
            copyQuote ('\\' : x    : xs) = '\\' : x    : copyQuote xs
            copyQuote ('\'' : '\'' : xs) = '\'' : '\'' : copyQuote xs
            copyQuote ('\''        : xs) = '\''        : copyMain False xs
            copyQuote (x           : xs) = x : copyQuote xs

            -- Copy identifiers (" ... ") literally
            copyIdentifier :: String -> String
            copyIdentifier []              = []
            copyIdentifier ('"'      : xs) = '"' : copyMain False xs
            copyIdentifier ('\\' : x : xs) = '\\' : x : copyIdentifier xs
            copyIdentifier (x        : xs) = x : copyIdentifier xs

    interpolate :: String -> TH.Exp
    interpolate = fromMaybe nullExp . runParse . parse
        where
            nullExp :: TH.Exp
            nullExp = stringE ""

            parse :: String -> (String, Maybe TH.Exp)
            parse []               = ("", Nothing)
            parse ('$' : '{' : xs) =
                case parseQuote xs of
                    Nothing -> 
                        prependChar '$' (prependChar '{' (parse xs))
                    Just ex  -> ("", Just ex)
            parse (x : xs)         = prependChar x $ parse xs

            parseQuote :: String -> Maybe TH.Exp
            parseQuote [] = Nothing
            parseQuote (x:xs)
                | (Char.isLower x) || (x == '_') = parseIdent [x] xs
                | otherwise                      = Nothing

            parseIdent :: String -> String -> Maybe TH.Exp
            parseIdent _   []         = Nothing
            parseIdent acc ('}' : xs) =
                maybeConcat
                    (Just (TH.SigE
                            (TH.AppE
                                (TH.VarE 'Text.unpack)
                                (TH.VarE (TH.mkName (reverse acc))))
                            (TH.ConT ''String)))
                    (runParse (parse xs))
            parseIdent acc (x : xs)
                | (Char.isAlphaNum x) || (x == '_') = parseIdent (x : acc) xs
                | otherwise                         = Nothing


            runParse :: (String, Maybe TH.Exp) -> Maybe TH.Exp
            runParse (s, me) = maybeConcat (stringLit s) me

            maybeConcat :: Maybe TH.Exp -> Maybe TH.Exp -> Maybe TH.Exp
            maybeConcat Nothing   Nothing   = Nothing
            maybeConcat me1       Nothing   = me1
            maybeConcat Nothing   me2       = me2
            maybeConcat (Just e1) (Just e2) =
                Just $ (TH.VarE '(++) `TH.AppE` e1) `TH.AppE` e2

            stringLit :: String -> Maybe TH.Exp
            stringLit s
                | Prelude.null s = Nothing
                | otherwise      = Just $ stringE s 

            prependChar :: Char
                            -> (String, Maybe TH.Exp)
                            -> (String, Maybe TH.Exp)
            prependChar c (s, e) = ((c : s), e)

