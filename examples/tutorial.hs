{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Database.Sql.Simple
import Database.Sql.Simple.SQLite
import Database.Sql.Simple.PostgreSQL
import Database.Sql.Simple.Pool

-- you must specify 1st type variable of Sql Monad.
-- by explicit type signature,
testQuery :: (ToRow conn (Only Int), FromRow conn (Only Int), Backend conn) 
          => conn -> Sql '[SQLite, PostgreSQL] [Int]
testQuery c = do
    execute_ c "CREATE TABLE test (id int)"
    execute  c "INSERT INTO test VALUES (?)" (Only (1 :: Int))
    map fromOnly <$> query_ c "SELECT * FROM test"

-- or sql function(testQuery' equivalent to testQuery).
testQuery' c = sql (sqlite +:+ postgreSQL) $ do
    execute_ c "CREATE TABLE test (id int)"
    execute  c "INSERT INTO test VALUES (?)" (Only (1 :: Int))
    i <- map fromOnly <$> query_ c "SELECT * FROM test"
    return (i :: [Int])

-- you can specify backend specific Query.
specificQuery :: Backend conn => conn -> Sql '[SQLite, PostgreSQL] ()
specificQuery c =
    execute_ c (specify sqlite "[sqlite query]" "[common query]")

main :: IO ()
main = do
    -- l <- withConnection ("test.sqlite3" :: ConnectInfo SQLite) testQuery
    l <- withConnection (ConnectionPool def "test.sqlite3" :: ConnectInfo (Pool SQLite)) testQuery
    -- l <- withConnection (def :: ConnectInfo PostgreSQL) testQuery
    print l
