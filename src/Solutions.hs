module Solutions (runSolution, runInteractive) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Day1 as Day1
import qualified Day5 as Day5
import qualified Day5Interactive as Day5Interactive

solutions :: IntMap (T.Text -> IO ())
solutions =
  M.fromList
    [ (1, Day1.program)
    , (5, Day5.program)
    ]
interactive :: IntMap (T.Text -> IO ())
interactive =
  M.fromList
    [ (5, Day5Interactive.program)
    ]

runSolution :: Int -> T.Text -> IO ()
runSolution = run solutions

runInteractive :: Int -> T.Text -> IO ()
runInteractive = run interactive

run :: IntMap (T.Text -> IO ()) -> Int -> T.Text -> IO ()
run m day input
  | day >= 1 && day <= 24 = case M.lookup day m of
      Just program -> program input
      Nothing -> putStrLn $ "solution for day " ++ show day ++ " not found"
  | otherwise = putStrLn $ "day out of range: " ++ show day
