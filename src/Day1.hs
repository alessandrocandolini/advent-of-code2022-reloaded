module Day1 where
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Answer = Answer deriving (Eq, Show)

program :: T.Text -> IO ()
program = print . logic

logic :: T.Text -> Answer
logic = const Answer