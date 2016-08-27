module ElfMagic where

import qualified Data.ByteString.Lazy as L

{-
 - Try to determine if a file is an ELF object file.
 - - ie: the format used for executables on almost all modern Unix systems
 -
 - Approach:
 - - look at the first four bytes in the file and see if matches a specific
 -   sequence of bytes. A byte sequence that identifies a file's type is
 -   often known as a magic number
 -}

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

hasElfMagic' :: L.ByteString -> Bool
hasElfMagic' = (==) elfMagic . L.take 4
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return $ hasElfMagic content

isElfFile' :: FilePath -> IO Bool
isElfFile' = (>>= (return . hasElfMagic)) . L.readFile
