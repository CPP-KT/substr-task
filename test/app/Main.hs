module Main where

import Lib
    ( TestResolution(TestResolution),
      Test(Test),
      TestData(RegularTest, GenerateEnoentTest, GenerateReadErrTest,
               GenerateBadArgsTest),
      runParTests )
import System.Environment ( getArgs )
import Control.Monad ( forM, forM_, replicateM )
import Control.Monad.Random ( Rand, RandomGen, evalRand, getRandom, getRandomR, mkStdGen )
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

data RandomGen g => RandomStringParams g = RSP { maxLength  :: Int
                                               , randomChar :: Rand g Char
                                               }

genRandomString :: RandomGen g => RandomStringParams g -> Rand g String
genRandomString (RSP maxLength randomChar) = do
  len <- getRandomR (1, maxLength)
  replicateM len randomChar

genRandomTest :: RandomGen g => RandomStringParams g -> RandomStringParams g -> String -> Rand g Test
genRandomTest dataParams queryParams testName = do
  testData <- genRandomString dataParams
  testQuery <- genRandomString queryParams
  return $ Test (RegularTest testData testQuery) testName

genRandomTests :: RandomGen g => Rand g [Test]
genRandomTests = do
  simpleTestData <- let simpleChar = getRandomR ('a', 'c')
                    in genRandomTests' 2000 "simple random test" (RSP 20 simpleChar) (RSP 8 simpleChar)
  binaryTestData <- let anyChar = getRandomR ('\0', '\255')
                        nonZeroChar = getRandomR ('\1', '\255')
                    in genRandomTests' 2000 "binary random test" (RSP 10 anyChar) (RSP 4 nonZeroChar)
  return $ simpleTestData ++ binaryTestData
  where
    genRandomTests' :: RandomGen g => Int -> String -> RandomStringParams g -> RandomStringParams g -> Rand g [Test]
    genRandomTests' n testNamePrefix dataParams queryParams =
      forM [1..n] $ \i -> genRandomTest dataParams queryParams (testNamePrefix ++ " " ++ show i)

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
            ++ evalRand genRandomTests (mkStdGen 1337)
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
