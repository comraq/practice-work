import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Environment

main = do
  ( fn1:fn2:_ ) <- getArgs
  copyFile fn1 fn2

copyFile          :: FilePath -> FilePath -> IO ()
copyFile src dest =  do
  contents <- B.readFile src
  B.writeFile dest contents
