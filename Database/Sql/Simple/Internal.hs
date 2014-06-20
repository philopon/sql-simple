{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Exts (Constraint)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Applicative
import qualified Database.PostgreSQL.Simple as PSql
import qualified Database.PostgreSQL.Simple.ToRow as PSql
import qualified Database.PostgreSQL.Simple.FromRow as PSql
import qualified Database.PostgreSQL.Simple.ToField as PSql
import qualified Database.PostgreSQL.Simple.FromField as PSql
import qualified Database.PostgreSQL.Simple.Types as PSql

import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Proxy
import Data.Typeable
import Data.String
import qualified Data.HashMap.Strict as H

data Query = Query T.Text (H.HashMap TypeRep T.Text)

instance IsString Query where
    fromString s = Query (T.pack s) H.empty

getQuery :: TypeRep -> Query -> T.Text
getQuery t (Query d h) = H.lookupDefault d t h

newtype Only a = Only { fromOnly :: a }

data h :. t = h :. t
infixr 3 :.

instance PSql.ToField a => PSql.ToRow (Only a) where
    toRow (Only v) = PSql.toRow $ PSql.Only v

instance PSql.FromField a => PSql.FromRow (Only a) where
    fromRow = Only . PSql.fromOnly <$> PSql.fromRow

instance (PSql.ToRow a, PSql.ToRow b) => PSql.ToRow (a :. b) where
    toRow (a :. b) = PSql.toRow $ a PSql.:. b

instance (PSql.FromRow a, PSql.FromRow b) => PSql.FromRow (a :. b) where
    fromRow = (\(a PSql.:. b) -> a :. b) <$> PSql.fromRow

instance SQLite.ToField a => SQLite.ToRow (Only a) where
    toRow (Only v) = SQLite.toRow $ SQLite.Only v

instance SQLite.FromField a => SQLite.FromRow (Only a) where
    fromRow = Only <$> SQLite.field

instance (SQLite.ToRow a, SQLite.ToRow b) => SQLite.ToRow (a :. b) where
    toRow (a :. b) = SQLite.toRow $ a SQLite.:. b

instance (SQLite.FromRow a, SQLite.FromRow b) => SQLite.FromRow (a :. b) where
    fromRow = (\(a SQLite.:. b) -> a :. b) <$> SQLite.fromRow

newtype Sql (l :: [*]) a = Sql { unSql :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO, Typeable, MonadThrow, MonadCatch, MonadMask)

class Elem a (as :: [*])
instance Elem a (a ': as)
instance Elem a as => Elem a' (a ': as)

withConnection :: (Backend c, Elem c l) => ConnectInfo c -> proxy l -> (c -> Sql l a) -> IO a
withConnection i _ f = bracket (connect i) close (unSql . f)

class Backend conn where
    data ConnectInfo conn
    type ToRow conn   :: * -> Constraint
    type FromRow conn :: * -> Constraint
  
    connect :: ConnectInfo conn -> IO conn
    close   :: conn -> IO ()
    
    execute  :: ToRow conn q => conn -> Query -> q -> Sql c ()
    execute_ :: conn -> Query -> Sql c ()
  
    query    :: (FromRow conn r, ToRow conn q) => conn -> Query -> q -> Sql c [r]
    query_   :: FromRow conn r => conn -> Query -> Sql c [r]
  
    begin    :: conn -> Sql c ()
    commit   :: conn -> Sql c ()
    rollback :: conn -> Sql c ()
    withTransaction :: conn -> Sql c a -> Sql c a
    withTransaction c action = mask $ \restore -> do
        begin c
        r <- restore action `onException` rollback c
        commit c
        return r

data PostgreSQL = PostgreSQL PSql.Connection
    deriving Typeable

instance Backend PostgreSQL where
    newtype ConnectInfo PostgreSQL = PSqlConnectInfo PSql.ConnectInfo
    type ToRow   PostgreSQL = PSql.ToRow
    type FromRow PostgreSQL = PSql.FromRow

    connect (PSqlConnectInfo i) = PostgreSQL <$> PSql.connect i
    close   (PostgreSQL      c) = PSql.close c

    execute  (PostgreSQL c) t q = void . Sql $ PSql.execute  c (psqlQuery t) q
    execute_ (PostgreSQL c) t   = void . Sql $ PSql.execute_ c (psqlQuery t)

    query    (PostgreSQL c) t q = Sql $ PSql.query  c (psqlQuery t) q
    query_   (PostgreSQL c) t   = Sql $ PSql.query_ c (psqlQuery t)

    begin    (PostgreSQL c) = Sql $ PSql.begin c
    commit   (PostgreSQL c) = Sql $ PSql.commit c
    rollback (PostgreSQL c) = Sql $ PSql.rollback c

psqlQuery :: Query -> PSql.Query
psqlQuery = PSql.Query . T.encodeUtf8 . getQuery (typeOf $ PostgreSQL undefined)

data SQLite = SQLite SQLite.Connection
    deriving Typeable

sqliteQuery :: Query -> SQLite.Query
sqliteQuery = SQLite.Query . getQuery (typeOf $ SQLite undefined)

instance Backend SQLite where
    newtype ConnectInfo SQLite = SQLiteConnectInfo String
    type ToRow   SQLite = SQLite.ToRow
    type FromRow SQLite = SQLite.FromRow

    connect (SQLiteConnectInfo i) = SQLite <$> SQLite.open i
    close   (SQLite c) = SQLite.close c

    execute  (SQLite c) t q = Sql $ SQLite.execute  c (sqliteQuery t) q
    execute_ (SQLite c) t   = Sql $ SQLite.execute_ c (sqliteQuery t)

    query    (SQLite c) t q = Sql $ SQLite.query  c (sqliteQuery t) q
    query_   (SQLite c) t   = Sql $ SQLite.query_ c (sqliteQuery t)

    begin    c = execute_ c "BEGIN TRANSACTION"
    commit   c = execute_ c "COMMIT TRANSACTION"
    rollback c = execute_ c "ROLLBACK TRANSACTION"

sqlite :: Proxy '[SQLite]
sqlite = Proxy

postgreSQL :: Proxy '[PostgreSQL]
postgreSQL = Proxy

psql :: Proxy '[PostgreSQL]
psql = postgreSQL

type family (a :: [k]) ++ (b :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': as ++ bs

(+:+) :: Proxy a -> Proxy b -> Proxy (a ++ b)
_ +:+ _ = Proxy

test i = withConnection i (sqlite +:+ psql) $ \c -> do
    execute_ c "CREATE TABLE test (id int)"
    execute  c "INSERT INTO test VALUES (?)" (Only (1 :: Int))
    map fromOnly <$> query_ c "SELECT * FROM test"
