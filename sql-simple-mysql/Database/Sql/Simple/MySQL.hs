{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Sql.Simple.MySQL
    ( MySQL
    , ConnectInfo(..)
    , mySQL
    , module Data.Default.Class
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Word
import Database.Sql.Simple.Internal
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Types as MySQL
import qualified Database.MySQL.Simple.Param as MySQL
import qualified Database.MySQL.Simple.Result as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Database.MySQL.Base as B
import Data.Default.Class
#if !MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif

data MySQL = MySQL MySQL.Connection
    deriving Typeable

instance MySQL.Param a => MySQL.QueryParams (Only a) where
    renderParams (Only v) = MySQL.renderParams $ MySQL.Only v

instance MySQL.Result a => MySQL.QueryResults (Only a) where
    convertResults a b = Only . MySQL.fromOnly $ MySQL.convertResults a b

instance (MySQL.QueryParams a, MySQL.QueryParams b) => MySQL.QueryParams (a :. b) where
    renderParams (a :. b) = MySQL.renderParams a ++ MySQL.renderParams b

instance (MySQL.QueryResults a, MySQL.QueryResults b) => MySQL.QueryResults (a :. b) where
    convertResults a b = MySQL.convertResults a b

instance Backend MySQL where
    data ConnectInfo MySQL = ConnectInfo
        { connectHost     :: String
        , connectPort     :: Word16
        , connectUser     :: String
        , connectPassword :: String
        , connectDatabase :: String
        , connectOptions  :: [B.Option]
        , connectPath     :: FilePath
        , connectSSL      :: Maybe B.SSLInfo
        } deriving (Eq, Read, Show)
    type ToRow   MySQL = MySQL.QueryParams
    type FromRow MySQL = MySQL.QueryResults

    connect (ConnectInfo h p u w d o t s) =
        MySQL <$> MySQL.connect (MySQL.ConnectInfo h p u w d o t s)
    close   (MySQL      c) = MySQL.close c

    execute  (MySQL c) t q = void . Sql $ MySQL.execute  c (mySqlQuery t) q
    execute_ (MySQL c) t   = void . Sql $ MySQL.execute_ c (mySqlQuery t)

    query    (MySQL c) t q = Sql $ MySQL.query  c (mySqlQuery t) q
    query_   (MySQL c) t   = Sql $ MySQL.query_ c (mySqlQuery t)

    begin    c = execute_ c "start transaction"
    commit   (MySQL c) = Sql $ B.commit c
    rollback (MySQL c) = Sql $ B.rollback c

instance Default (ConnectInfo MySQL) where
    def = ConnectInfo h p u w d o t s
      where
        MySQL.ConnectInfo h p u w d o t s = MySQL.defaultConnectInfo

mySqlQuery :: Query -> MySQL.Query
mySqlQuery = MySQL.Query . T.encodeUtf8 . getQuery (typeOf (undefined :: MySQL))

mySQL :: Proxy '[MySQL]
mySQL = Proxy
