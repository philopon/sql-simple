{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.Sql.Simple
    ( -- * data type
      -- ** query
      I.Query
    , I.specify
      -- ** parameter
    , I.ToRow
    , I.FromRow
    , I.Only(..)
    , (I.:.)((:.))
      -- ** other
    , I.Sql
    , I.Elem
    , I.Backend
      -- * connection
    , I.ConnectInfo
    , I.withConnection
    , connect
    , close
      -- * execute query
    , execute, execute_
    , query,   query_
    , fold,    fold_
    , forEach, forEach_
      -- ** transaction
    , begin
    , commit
    , rollback
    , withTransaction
    -- * specify backend
    -- | 
    -- @ 
    --  sql (sqlite +:+ postgresql) $ query
    -- @
    , I.sql
    , (I.+:+)
    , type (I.++)
    ) where

import qualified Database.Sql.Simple.Internal as I

connect :: I.Backend b => I.ConnectInfo b -> IO b
connect = I.connect

close :: I.Backend b => b -> IO ()
close = I.close

execute :: (I.ToRow b q, I.Backend b) => b -> I.Query -> q -> I.Sql bs ()
execute = I.execute

execute_ :: I.Backend b => b -> I.Query -> I.Sql bs ()
execute_ = I.execute_

query :: (I.Backend b, I.FromRow b r, I.ToRow b q) => b -> I.Query -> q -> I.Sql bs [r]
query = I.query

query_ :: (I.FromRow b r, I.Backend b) => b -> I.Query -> I.Sql bs [r]
query_ = I.query_

fold :: (I.Backend b, I.FromRow b r, I.ToRow b q)
     => b -> I.Query -> q -> a -> (a -> r -> IO a) -> IO a
fold = I.fold

fold_ :: (I.Backend b, I.FromRow b r)
      => b -> I.Query -> a -> (a -> r -> IO a) -> IO a
fold_ = I.fold_

forEach :: (I.Backend b, I.FromRow b r, I.ToRow b q)
        => b -> I.Query -> q -> (r -> IO ()) -> IO ()
forEach = I.forEach

forEach_ :: (I.Backend b, I.FromRow b r)
         => b -> I.Query -> (r -> IO ()) -> IO ()
forEach_ = I.forEach_

begin, commit, rollback :: I.Backend b => b -> I.Sql bs ()
begin = I.begin
commit = I.commit
rollback = I.rollback

withTransaction :: I.Backend b => b -> I.Sql bs a -> I.Sql bs a
withTransaction = I.withTransaction
