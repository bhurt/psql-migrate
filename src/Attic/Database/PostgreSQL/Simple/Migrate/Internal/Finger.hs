{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Finger
-- Description : Calculate migration fingerprints.
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

module Database.PostgreSQL.Simple.Migrate.Internal.Finger (
    makeFingerprint
) where

    import qualified Data.Binary                      as Binary
    import           Data.ByteString                  (ByteString)
    import qualified Data.ByteString.Base64.URL       as Base64
    import qualified Data.ByteString.Lazy             as Lazy
    import qualified Data.Digest.Pure.SHA             as Hash
    import           Data.Text                        (Text)
    import qualified Data.Text.Encoding               as Encoding
    import qualified Database.PostgreSQL.Simple.Types as PQ

    -- | Calculate the fingerprint of a given migration command.
    makeFingerprint :: PQ.Query -> Text
    makeFingerprint cmd =
        let q0 :: ByteString
            q0 = PQ.fromQuery cmd

            q :: Lazy.ByteString
            q = Lazy.fromStrict q0

            dig :: Hash.Digest Hash.SHA256State
            dig = Hash.sha256 q

            bs0 :: Lazy.ByteString
            bs0 = Binary.encode dig

            bs :: ByteString
            bs = Lazy.toStrict bs0

            b64 :: ByteString
            b64 = Base64.encode bs
        in
        Encoding.decodeUtf8 b64

