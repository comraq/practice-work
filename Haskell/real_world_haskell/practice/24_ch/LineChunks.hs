module LineChunks (chunkedReadWith) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, liftM)
import Control.Parallel.Strategies (NFData, rdeepseq)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities)
import System.IO

data ChunkSpec = CS {
  chunkOffset :: !Int64
, chunkLength :: !Int64
} deriving (Eq, Show)

{-
 - * Avoid starvation by explicitly closing file handles.
 - * Allow multiple threads to read different chunks at once by supplying each
 -   one with a distinct file handle, all reading the same file.
 - * Use 'rdeepseq to force all processing/reading to complete before returning
 -   and closing the file handles
 -}
withChunks :: NFData a =>
              (FilePath -> IO [ChunkSpec])
           -> ([LB.ByteString] -> a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
  (chunks, handles) <- chunkedRead chunkFunc path
  let r = process chunks
  (rdeepseq r `seq` return r) `finally` mapM_ hClose handles

chunkedReadWith :: NFData a =>
                   ([LB.ByteString] -> a) -> FilePath -> IO a
chunkedReadWith func path =
  withChunks (lineChunks (numCapabilities * 4)) func path

chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([LB.ByteString], [Handle])
chunkedRead chunkFunc path = do
  chunks <- chunkFunc path
  liftM unzip . forM chunks $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek $ fromIntegral (chunkOffset spec)
    chunk <- LB.take (chunkLength spec) `liftM` LB.hGetContents h
    return (chunk, h)

{-
 - Breaking a large file into chunks, while also ensuring that each chunk
 - ends on a newline boundary.
 -
 - First seek the approximate position of the end of a chunk, then scan
 - forwards until reaching a newline character. Then start next chunk after
 - the newline.
 -}
lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral `liftM` hFileSize h
    let chunkSize = totalSize `div` fromIntegral numChunks
        findChunks offset = do
          let newOffset = offset + chunkSize
          hSeek h AbsoluteSeek $ fromIntegral newOffset
          let findNewline off = do
                eof <- hIsEOF h
                if eof
                then return [CS offset (totalSize - offset)]
                else do
                  bytes <- LB.hGet h 4096
                  case LB.elemIndex '\n' bytes of
                    Just n -> do
                      chunks@(c:_) <- findChunks (off + n + 1)
                      let coff = chunkOffset c
                      return $ CS offset (coff - offset):chunks

                    Nothing -> findNewline $ off + LB.length bytes

          findNewline newOffset

    findChunks 0
