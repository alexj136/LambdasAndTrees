module TestUtil where

data Test = Test String Bool deriving (Show, Eq, Ord)

runTests :: [Test] -> IO Bool
runTests tests = do
    allResults <- sequence $ map runTest tests
    return $ and allResults

runTest :: Test -> IO Bool
runTest test = case test of
    Test _      True  -> return True
    Test testDesc False -> do
        putStrLn $ "Test failed: " ++ testDesc
        return False
