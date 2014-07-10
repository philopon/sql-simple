{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Sql.Simple.Pool where

import Control.Applicative
import Control.Monad.Trans.Control
import Data.Typeable
import Data.Default.Class
import Database.Sql.Simple.Internal
import Data.Time.Clock
import qualified Data.Pool as Pool

data Backend b => Pool b = Pool (Pool.Pool b)
    deriving (Typeable)

instance Elem (Pool a) (a ': as)

data PoolConfig = PoolConfig
    { numStripes   :: Int
    , idleTime     :: NominalDiffTime
    , maxResources :: Int
    } deriving (Show, Typeable)

instance Default PoolConfig where
    def = PoolConfig 1 20 100

instance Backend b => Backend (Pool b) where
    data ConnectInfo (Pool b) = ConnectionPool
        { poolConfig  :: PoolConfig
        , connectInfo :: ConnectInfo b 
        }
    type ToRow   (Pool b) = ToRow b
    type FromRow (Pool b) = FromRow b

    connect (ConnectionPool PoolConfig{..} ci) = 
        Pool <$> Pool.createPool (connect ci) close numStripes idleTime maxResources

    close (Pool p) = Pool.destroyAllResources p

    execute  (Pool p) t q = Pool.withResource p $ \c -> execute  c t q
    execute_ (Pool p) t   = Pool.withResource p $ \c -> execute_ c t
    query    (Pool p) t q = Pool.withResource p $ \c -> query    c t q
    query_   (Pool p) t   = Pool.withResource p $ \c -> query_   c t

    fold  (Pool p) t q a f = Pool.withResource p $ \c -> fold  c t q a f
    fold_ (Pool p) t   a f = Pool.withResource p $ \c -> fold_ c t   a f

withPool :: (Backend b, MonadBaseControl IO m) => Pool b -> (b -> m a) -> m a
withPool (Pool p) = Pool.withResource p

transaction :: Transaction b => Pool b -> (b -> Sql c a) -> Sql c a
transaction p m = withPool p $ \c -> withTransaction c (m c)
