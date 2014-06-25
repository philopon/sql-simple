{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Sql.Simple.PostgreSQL
    ( PostgreSQL
    , ConnectInfo(..)
    , backend
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Proxy
import Data.Word
import Database.Sql.Simple.Internal
import qualified Database.PostgreSQL.Simple as PSql
import qualified Database.PostgreSQL.Simple.ToRow as PSql
import qualified Database.PostgreSQL.Simple.FromRow as PSql
import qualified Database.PostgreSQL.Simple.ToField as PSql
import qualified Database.PostgreSQL.Simple.FromField as PSql
import qualified Database.PostgreSQL.Simple.Types as PSql

data PostgreSQL = PostgreSQL PSql.Connection
    deriving Typeable

instance PSql.ToField a => PSql.ToRow (Only a) where
    toRow (Only v) = PSql.toRow $ PSql.Only v

instance PSql.FromField a => PSql.FromRow (Only a) where
    fromRow = Only . PSql.fromOnly <$> PSql.fromRow

instance (PSql.ToRow a, PSql.ToRow b) => PSql.ToRow (a :. b) where
    toRow (a :. b) = PSql.toRow $ a PSql.:. b

instance (PSql.FromRow a, PSql.FromRow b) => PSql.FromRow (a :. b) where
    fromRow = (\(a PSql.:. b) -> a :. b) <$> PSql.fromRow

instance Backend PostgreSQL where
    data ConnectInfo PostgreSQL = ConnectInfo
        { connectHost :: String
        , connectPort :: Word16
        , connectUser :: String
        , connectPassword :: String
        , connectDatabase :: String
        }
    type ToRow   PostgreSQL = PSql.ToRow
    type FromRow PostgreSQL = PSql.FromRow

    connect (ConnectInfo h p u w d) =
        PostgreSQL <$> PSql.connect (PSql.ConnectInfo h p u w d)
    close   (PostgreSQL      c) = PSql.close c

    execute  (PostgreSQL c) t q = void . Sql $ PSql.execute  c (psqlQuery t) q
    execute_ (PostgreSQL c) t   = void . Sql $ PSql.execute_ c (psqlQuery t)

    query    (PostgreSQL c) t q = Sql $ PSql.query  c (psqlQuery t) q
    query_   (PostgreSQL c) t   = Sql $ PSql.query_ c (psqlQuery t)

    begin    (PostgreSQL c) = Sql $ PSql.begin c
    commit   (PostgreSQL c) = Sql $ PSql.commit c
    rollback (PostgreSQL c) = Sql $ PSql.rollback c

psqlQuery :: Query -> PSql.Query
psqlQuery = PSql.Query . T.encodeUtf8 . getQuery (typeOf (undefined :: PostgreSQL))

backend :: Proxy '[PostgreSQL]
backend = Proxy
