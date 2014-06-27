{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Database.Sql.Simple.SQLite
    ( SQLite
    , ConnectInfo(..)
    , sqlite
    ) where

import Control.Applicative
import Data.Typeable
import Database.Sql.Simple.Internal
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import Data.String
#if !MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif

data SQLite = SQLite SQLite.Connection
    deriving Typeable

sqliteQuery :: Query -> SQLite.Query
sqliteQuery = SQLite.Query . getQuery (typeOf (undefined :: SQLite))

instance Backend SQLite where
    newtype ConnectInfo SQLite = ConnectInfo String deriving (Eq, Read, Show)
    type ToRow   SQLite = SQLite.ToRow
    type FromRow SQLite = SQLite.FromRow

    connect (ConnectInfo i) = SQLite <$> SQLite.open i
    close   (SQLite c) = SQLite.close c

    execute  (SQLite c) t q = Sql $ SQLite.execute  c (sqliteQuery t) q
    execute_ (SQLite c) t   = Sql $ SQLite.execute_ c (sqliteQuery t)

    query    (SQLite c) t q = Sql $ SQLite.query  c (sqliteQuery t) q
    query_   (SQLite c) t   = Sql $ SQLite.query_ c (sqliteQuery t)

    begin    c = execute_ c "BEGIN TRANSACTION"
    commit   c = execute_ c "COMMIT TRANSACTION"
    rollback c = execute_ c "ROLLBACK TRANSACTION"

instance IsString (ConnectInfo SQLite) where
    fromString = ConnectInfo

sqlite :: Proxy '[SQLite]
sqlite = Proxy

instance SQLite.ToField a => SQLite.ToRow (Only a) where
    toRow (Only v) = SQLite.toRow $ SQLite.Only v

instance SQLite.FromField a => SQLite.FromRow (Only a) where
    fromRow = Only <$> SQLite.field

instance (SQLite.ToRow a, SQLite.ToRow b) => SQLite.ToRow (a :. b) where
    toRow (a :. b) = SQLite.toRow $ a SQLite.:. b

instance (SQLite.FromRow a, SQLite.FromRow b) => SQLite.FromRow (a :. b) where
    fromRow = (\(a SQLite.:. b) -> a :. b) <$> SQLite.fromRow


