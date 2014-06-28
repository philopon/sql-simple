{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Sql.Simple.MySQL
    ( MySQL
    , ConnectInfo(..)
    , mySQL
    , module Data.Default.Class
    , QueryResultsN(..)
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

class MySQL.QueryResults a => QueryResultsN a where
  queryLength :: proxy a -> Int

instance MySQL.Result a => QueryResultsN (Only a) where queryLength _ = 1
instance (MySQL.Result a, MySQL.Result b) => QueryResultsN (a,b) where queryLength _ = 2
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c) => QueryResultsN (a,b,c) where queryLength _ = 3
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d) => QueryResultsN (a,b,c,d) where queryLength _ = 4
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e) => QueryResultsN (a,b,c,d,e) where queryLength _ = 5
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e, MySQL.Result f) => QueryResultsN (a,b,c,d,e,f) where queryLength _ = 6
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e, MySQL.Result f, MySQL.Result g) => QueryResultsN (a,b,c,d,e,f,g) where queryLength _ = 7
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e, MySQL.Result f, MySQL.Result g, MySQL.Result h) => QueryResultsN (a,b,c,d,e,f,g,h) where queryLength _ = 8
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e, MySQL.Result f, MySQL.Result g, MySQL.Result h, MySQL.Result i) => QueryResultsN (a,b,c,d,e,f,g,h,i) where queryLength _ = 9
instance (MySQL.Result a, MySQL.Result b, MySQL.Result c, MySQL.Result d, MySQL.Result e, MySQL.Result f, MySQL.Result g, MySQL.Result h, MySQL.Result i, MySQL.Result j) => QueryResultsN (a,b,c,d,e,f,g,h,i,j) where queryLength _ = 10

instance (QueryResultsN a, QueryResultsN b) => QueryResultsN (a :. b) where
    queryLength _ = queryLength (Proxy :: Proxy a) + queryLength (Proxy :: Proxy b)

instance (QueryResultsN a, QueryResultsN b) => MySQL.QueryResults (a :. b) where
    convertResults fs bs = 
        let len = queryLength (Proxy :: Proxy a)
            (fa, fb) = splitAt len fs
            (ba, bb) = splitAt len bs
        in MySQL.convertResults fa ba :. MySQL.convertResults fb bb

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

    fold  (MySQL c) q = MySQL.fold  c (mySqlQuery q)
    fold_ (MySQL c) q = MySQL.fold_ c (mySqlQuery q)

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
