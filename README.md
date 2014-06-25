
test i = withConnection i $ \c -> sql (sqlite +:+ psql) $ do
    execute_ c "CREATE TABLE test (id int)"
    execute  c "INSERT INTO test VALUES (?)" (Only (1 :: Int))
    map fromOnly <$> query_ c "SELECT * FROM test"
