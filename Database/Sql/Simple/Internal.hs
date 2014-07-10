{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Database.Sql.Simple.Internal where

import GHC.Exts (Constraint)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Applicative

import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import Data.String
import qualified Data.HashMap.Strict as H

data Query = Query T.Text (H.HashMap TypeRep T.Text)
    deriving (Show, Eq)

newtype Sql (l :: [*]) a = Sql { unSql :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadBase IO)

instance MonadBaseControl IO (Sql l) where
    newtype StM (Sql l) a = StMSql { unStMSql :: StM IO a }
    liftBaseWith f = Sql $ 
        liftBaseWith (\run -> f $ liftM StMSql . run . unSql)
    restoreM = Sql . restoreM . unStMSql

instance IsString Query where
    fromString s = Query (T.pack s) H.empty

getQuery :: TypeRep -> Query -> T.Text
getQuery t (Query d h) = H.lookupDefault d t h

newtype Only a = Only { fromOnly :: a }

data h :. t = h :. t
infixr 3 :.

class Elem a (as :: [*])
instance Elem a (a ': as)
instance Elem a as => Elem a (a' ': as)

withConnection :: (Backend b, Elem b bs) => ConnectInfo b -> (b -> Sql bs a) -> IO a
withConnection i f = bracket (connect i) close (unSql . f)

-- | specify sql backends.
--
sql :: proxy bs -> Sql bs a -> Sql bs a
sql _ m = m

class Typeable b => Backend b where
    data ConnectInfo b
    type ToRow b   :: * -> Constraint
    type FromRow b :: * -> Constraint
  
    connect  :: ConnectInfo b -> IO b
    close    :: b -> IO ()
    
    execute  :: ToRow b q => b -> Query -> q -> Sql c ()
    execute_ :: b -> Query -> Sql c ()
  
    query    :: (FromRow b r, ToRow b q) => b -> Query -> q -> Sql c [r]
    query_   :: FromRow b r => b -> Query -> Sql c [r]

    fold     :: (FromRow b r, ToRow b q) => b -> Query -> q -> a -> (a -> r -> IO a) -> IO a
    fold_    :: FromRow b r => b -> Query -> a -> (a -> r -> IO a) -> IO a

    forEach  :: (FromRow b r, ToRow b q) => b -> Query -> q -> (r -> IO ()) -> IO ()
    forEach  c q qs = fold  c q qs () . const
    forEach_ :: FromRow b r => b -> Query -> (r -> IO ()) -> IO ()
    forEach_ c q    = fold_ c q () . const
  
class Backend b => Transaction b where
    begin    :: b -> Sql c ()
    commit   :: b -> Sql c ()
    rollback :: b -> Sql c ()
    withTransaction :: b -> Sql c a -> Sql c a
    withTransaction c action = mask $ \restore -> do
        begin c
        r <- restore action `onException` rollback c
        commit c
        return r

type family (a :: [k]) ++ (b :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': as ++ bs

-- | join sql backends.
(+:+) :: Proxy a -> Proxy b -> Proxy (a ++ b)
_ +:+ _ = Proxy

-- | add specified query string to Query.
--
-- example:
-- 
-- @
-- q = specify sqlite \"sqlite query\" \"common query\"
-- @
specify :: Backend b => proxy ((b :: *) ': '[]) -> T.Text -> Query -> Query
specify p q (Query t h) = Query t (H.insert (headt p) q h)
  where
    headt :: forall proxy a as. Typeable a => proxy ((a :: *) ': as) -> TypeRep
    headt _ = typeOf (undefined :: a)
