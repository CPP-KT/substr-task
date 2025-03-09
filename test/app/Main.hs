module Main where

import Lib
    ( TestResolution(TestResolution),
      Test(Test),
      TestData(RegularTest, GenerateEnoentTest, GenerateReadErrTest,
               GenerateBadArgsTest),
      runParTests )
import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Exception ( throwIO, Exception )

data TestFailedException = TestFailedException
  { failedTestName  :: String
  , failedTestCause :: String
  }

instance Show TestFailedException where
  show (TestFailedException name cause) = "Test \"" ++ name ++ "\" failed:\n" ++ cause

instance Exception TestFailedException

loadBibleDataset :: IO [Test]
loadBibleDataset = do
  bible <- readFile "assets/kjvbible.txt"
  return [ Test (RegularTest bible "lewd") "Bible test 1"
         , Test (RegularTest bible "foobarbaz") "Bible test 2"
         , Test (RegularTest (bible ++ "foobarbaz") "foobarbaz") "Bible test 3"
         ]

main :: IO ()
main = do
  bibleDataset <- loadBibleDataset
  let tests = [ Test (RegularTest "aaa" "aaa") "data equals query"
              , Test (RegularTest "aaa" "a") "simple char search - positive"
              , Test (RegularTest "aaa" "b") "simple char search - negative"
              , Test (RegularTest "ababar" "abar") "fake prefix match 1"
              , Test (RegularTest "aaabaaa" "aaaa") "fake prefix match 2"
              , Test GenerateEnoentTest "simple enoent run"
              , Test GenerateReadErrTest "simple read err test"
              , Test GenerateBadArgsTest "simple many args test"
              ]
            ++ bibleDataset
  args <- getArgs
  if length args /= 1 then do
    putStrLn "Path to student solution expected as the only one argument"
  else do
    let solution = head args
    results <- runParTests solution tests
    forM_ results checkResolution
  where
    checkResolution (TestResolution id (Left err)) = throwIO $ TestFailedException id err
    checkResolution (TestResolution id _) = putStrLn $ "Test \"" ++ id ++ "\" -- OK"
