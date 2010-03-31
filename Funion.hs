module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.FilePath.Posix
import System.Posix.IO
import System.Directory

import System.Fuse
import System(getArgs)
import System.Environment(withArgs)
import Control.Monad
import DirTools.DirTools
import Data.Maybe
import Data.List

type HT = ()

{-
OPEN QUESTIONS:
For now, I suppose that I could just specify the directories that I want to union on the command line.
What about making this more aggressive and having some way of specifying unioning rules?  Perhaps
via a DSL?

How should I present SymLinks?  
-}


{- TODO(nathan)

* CHANGE ARGUMENTS TO PROGRAM 
  Change it so that all arguments are assumed to be 'mine' unless somehow
  otherwise noted.  Possibly with "--" 

* LOOKUP
  Helper function which looks up the path in the existing file systems.  It should 
  return a struct that contains :
    name
    size
    owner/group
    permissions
    time
  if it's a directory, it should also have a list of its children.
-}


data FunionFS = FunionFS {
      funionEntryName     :: FilePath
    , funionActualPath    :: FilePath
    , funionVirtualPath   :: FilePath 
    , funionFileStat      :: FileStat
    , funionContents      :: [FunionFS]
    }
  deriving Show


dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents 

fileExists :: FilePath -> FilePath -> IO Bool
fileExists path name = doesFileExist $ path </> name

dirExists :: FilePath -> FilePath -> IO Bool
dirExists path name = doesDirectoryExist $ path </> name


getFileStats, getDirStats :: FilePath-> FilePath -> IO FunionFS
getFileStats path name = do
  let uri = path </> name
  status <- getFileStatus uri
  return FunionFS {
      funionEntryName   = name
    , funionActualPath  = uri
    , funionVirtualPath = ""
    , funionFileStat    = FileStat { statEntryType = RegularFile
        , statFileMode =  fileMode status
        , statLinkCount = linkCount status
        , statFileOwner = fileOwner status
        , statFileGroup = fileGroup status
        , statSpecialDeviceID = specialDeviceID status
        , statFileSize = fileSize status
        , statBlocks = 1
        , statAccessTime = accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
      }
     , funionContents = []
  }


getDirStats  path name = do
  let uri = path </> name
  status <- getFileStatus uri
  return FunionFS {
      funionEntryName   = name
    , funionActualPath  = uri
    , funionVirtualPath = ""
    , funionFileStat    = FileStat { statEntryType = Directory
        , statFileMode =  fileMode status
        , statLinkCount = linkCount status
        , statFileOwner = fileOwner status
        , statFileGroup = fileGroup status
        , statSpecialDeviceID = specialDeviceID status
        , statFileSize = fileSize status
        , statBlocks = 1
        , statAccessTime = accessTime status
        , statModificationTime = modificationTime status
        , statStatusChangeTime = statusChangeTime status
 
      }
     , funionContents = []
  }

readDir' :: FilePath -> IO (FunionFS)
readDir' path = do
  contents <- dirContents path
  --files <- filterM (doesFileExist) $ map (uri++) contents
  files <- filterM (fileExists path) contents
  fileList <- mapM (getFileStats path) files
  -- list of directories
  dirs <- filterM (dirExists path) contents
  dirList <- mapM (getDirStats path) dirs
  
  return $ FunionFS {
      funionEntryName   = takeFileName path
    , funionActualPath  = ""
    , funionVirtualPath = path
    , funionFileStat    = dirStat -- need to reflect time/permissions
    , funionContents    = fileList ++ dirList
  }
 


{- TODO(Nathan)
  Make it actually do unioning and also handle looking up a file
-}
funionLookUp :: [FilePath] -> FilePath -> IO (Maybe FunionFS) 
funionLookUp dirsToUnion path = do
  dirs  <- filterM (`dirExists` path) dirsToUnion
  dirList <- mapM (readDir'.( </> path)) dirs

  files <- filterM (`fileExists` path) dirsToUnion
  fileStats <- mapM (`getFileStats` path) files

  let contents = map funionContents dirList
  return $ if length fileStats > 0 then Just $head fileStats else Just FunionFS {
      funionEntryName   = takeFileName path
    , funionActualPath  = ""
    , funionVirtualPath = path
    , funionFileStat    = dirStat
    , funionContents    = concat contents
  }


main :: IO ()
main = do
  args <- getArgs
  let args' = map tail $ filter (\(x:xs) -> if x == '+' then True else False) args
  let args2 = filter(\(x:xs) -> if x == '+' then False else True) args
  putStrLn $ show args'
  withArgs args2 $ fuseMain (funionFSOps args')  defaultExceptionHandler


funionFSOps :: [FilePath] -> FuseOperations HT
funionFSOps dir = 
  defaultFuseOps{ fuseGetFileStat        = funionGetFileStat dir
                , fuseOpen               = funionOpen dir
                , fuseRead               = funionRead dir
                , fuseOpenDirectory      = funionOpenDirectory dir
                , fuseReadDirectory      = funionReadDirectory dir
                , fuseGetFileSystemStats = funionGetFileSystemStats dir
                }


funionGetFileStat :: [FilePath] -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat dirsToUnion (_:dir) = do 
    ctx <- getFuseContext
    (Just file) <- funionLookUp dirsToUnion dir 
    return $ Right $ funionFileStat file


funionOpen :: [FilePath] -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
funionOpen tree path mode flags
    | (path == "/afile") || (path == "/afile1") ||  (path == "/afile2") || (path == "/afile3") = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise       = return (Left eNOENT) 


funionOpenDirectory :: [FilePath] -> FilePath -> IO Errno
funionOpenDirectory dirsToUnion (_:path) = do -- return eOK --eNOENT
  extantDirs <- filterM (`dirExists` path) dirsToUnion 
  return $ if length extantDirs > 0 then eOK else eNOENT


funionGetFileSystemStats :: [FilePath]->String -> IO (Either Errno FileSystemStats)
funionGetFileSystemStats fileTree  str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5  -- IS THIS CORRECT?
    , fsStatFilesFree = 10 -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }


funionReadDirectory :: [FilePath] ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory dirsToUnion (_:dir) = do
    ctx <- getFuseContext
    entry <- funionLookUp dirsToUnion dir
    let contents = funionContents $ fromJust  entry
    let dirContents = map (\x -> ((funionEntryName x) :: String , funionFileStat x)) contents
    return $ Right $ [ (".", dirStat), ("..", dirStat)] ++ dirContents




-- #############################################################################
--                   ALL OF THIS SHOULD BE REMOVED
-- #############################################################################

myMessage :: B.ByteString
myMessage = B.pack "Hello there world.  This is a test of the nathan broadcast system\n"

{- TODO(nathan)
    Look up the path in the assoc list to get the real path to the file.  Then, read that block from the file.
-}
funionRead  :: [FilePath] -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
funionRead fileTree  path _ byteCount offset 
    | path == "/afile" = return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) myMessage
    | path == "/afile1" = return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) myMessage
    | path == "/afile2" = return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) myMessage
    | path == "/afile3" = return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) myMessage
    | otherwise       = return $ Left eNOENT




{-
    The following two should be completely removed and replaced with
    functions which wrap the statistics of the real files and directories
    in FileStats
-}


dirStat = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 5
                       , statFileOwner = 1000
                       , statFileGroup = 1000
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length myMessage 
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


