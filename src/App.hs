module App where

import Args (
  Args (Args),
  Command (..),
  GenerateArgs (GenerateArgs),
  StatsArgs (StatsArgs),
  parseArgs,
  readInput,
 )
import CodeGenerator (program)
import Options.Applicative (handleParseResult)
import Solutions (runInteractive, runSolution)
import Stats (program)
import System.Environment (getArgs)

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Command -> IO ()
program' (Solve (Args n input)) = readInput input >>= runSolution n
program' (Generate (GenerateArgs d)) = CodeGenerator.program d
program' (GetStats (StatsArgs year export)) = Stats.program year export
program' (Interactive (Args n input)) = readInput input >>= runInteractive n
