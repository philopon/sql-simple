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

module Database.Sql.Simple where

import GHC.Exts (Constraint)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Applicative

import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import Data.String
import qualified Data.HashMap.Strict as H

data Query = Query T.Text (H.HashMap TypeRep T.Text)

newtype Sql (l :: [*]) a = Sql { unSql :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO, Typeable, MonadThrow, MonadCatch, MonadMask)

instance IsString Query where
    fromString s = Query (T.pack s) H.empty

getQuery :: TypeRep -> Query -> T.Text
getQuery t (Query d h) = H.lookupDefault d t h

newtype Only a = Only { fromOnly :: a }

data h :. t = h :. t
infixr 3 :.

class Elem a (as :: [*])
instance Elem a (a ': as)
instance Elem a as => Elem a' (a ': as)

withConnection :: (Backend c, Elem c l) => ConnectInfo c -> (c -> Sql l a) -> IO a
withConnection i f = bracket (connect i) close (unSql . f)

sql :: proxy l -> Sql l a -> Sql l a
sql _ m = m

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

type family (a :: [k]) ++ (b :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': as ++ bs

(+:+) :: Proxy a -> Proxy b -> Proxy (a ++ b)
_ +:+ _ = Proxy

{-
test i = withConnection i $ \c -> sql (sqlite +:+ psql) $ do
    execute_ c "CREATE TABLE test (id int)"
    execute  c "INSERT INTO test VALUES (?)" (Only (1 :: Int))
    map fromOnly <$> query_ c "SELECT * FROM test"
    -}
