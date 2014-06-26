{-# LANGUAGE ExplicitNamespaces #-}

module Database.Sql.Simple
    ( -- * data type
      -- ** query
      Query
    , specify
      -- ** parameter
    , ToRow
    , FromRow
    , Only(..)
    , (:.)((:.))
      -- ** other
    , Sql
    , Elem
    , Backend
      -- * connection
    , ConnectInfo
    , withConnection
    , connect
    , close
      -- * execute query
    , execute
    , execute_
    , query
    , query_
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
    , sql
    , (+:+)
    , type (++)
    ) where

import Database.Sql.Simple.Internal
