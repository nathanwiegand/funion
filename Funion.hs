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

* LAZY UNIONING
  As it stands the directory views are unioned at time of program launch.  This
  should be changed to be done lazily.  In fact, it should probably be done 
  on-demand because of possile changes to the underlying file system.

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
  }

fileExists :: FilePath -> String -> IO Bool
fileExists path name = doesFileExist $ path </>  name

dirExists :: FilePath -> String -> IO Bool
dirExists path name = doesDirectoryExist $ path </> name


{-
funionReadUnderlyingDirectory :: FilePath -> IO (Maybe FunionFS)
funionReadUnderlyingDirectory path = do
  perm <- getPermissions path
  time <- getModificationTime path
  contents <- dirContents path
  files <- filterM (fileExists path) contents
   

funionLookUp :: [FunionFS] -> FilePath -> IO (Maybe FunionFS)
funionLookUp dirs path = do
  let root = head $ splitDirectories path
  let subdirs = 
-}


main :: IO ()
main = do
  args <- getArgs
  let args' = map tail $ filter (\(x:xs) -> if x == '+' then True else False) args
  let args2 = filter(\(x:xs) -> if x == '+' then False else True) args
  putStrLn $ show args'
  dirs <- mapM (`readDir` "") $ args'
  let tree = treeUnion "" dirs
  withArgs args2 $ fuseMain (funionFSOps tree)  defaultExceptionHandler


funionFSOps :: FSDirectory -> FuseOperations HT
funionFSOps dir = 
  defaultFuseOps{ fuseGetFileStat        = funionGetFileStat dir
                , fuseOpen               = funionOpen dir
                , fuseRead               = funionRead dir
                , fuseOpenDirectory      = funionOpenDirectory dir
                , fuseReadDirectory      = funionReadDirectory dir
                , fuseGetFileSystemStats = funionGetFileSystemStats dir
                }


funionGetFileStat :: FSDirectory -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat fileTree dir = do 
    ctx <- getFuseContext
    let (x:dir2) = dir
    let subtree = getSubTree dir2 fileTree

    return $ Right $ dirStat ctx


funionOpen :: FSDirectory -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
funionOpen tree path mode flags
    | (path == "/afile") || (path == "/afile1") ||  (path == "/afile2") || (path == "/afile3") = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise       = return (Left eNOENT) 


funionOpenDirectory :: FSDirectory -> FilePath -> IO Errno
funionOpenDirectory fileTree _ = return eOK --eNOENT


funionGetFileSystemStats :: FSDirectory->String -> IO (Either Errno FileSystemStats)
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


funionReadDirectory :: FSDirectory ->FilePath -> IO (Either Errno [(FilePath, FileStat)])
funionReadDirectory fileTree dir = do
    ctx <- getFuseContext
    let (x:dir2) = dir
    let subtree = getSubTree dir2 fileTree
    return $ Right $[ (".", dirStat ctx)
                     ,("..", dirStat ctx), ("test", fileStat ctx)] 
                ++  (map (\x-> ((fsEntryFileName $ fileStats x), fileStat ctx)) (dirFiles subtree))
                ++  (map (\x-> ((fsEntryFileName $ dirStats x), dirStat ctx)) (dirDirs subtree))
--                ++  (map (\x-> (fsEntryFileName $ fileStats x, fileStat ctx)) (dirFiles fileTree))








-- #############################################################################
--                   ALL OF THIS SHOULD BE REMOVED
-- #############################################################################

myMessage :: B.ByteString
myMessage = B.pack "Hello there world.  This is a test of the nathan broadcast system\n"

{- TODO(nathan)
    Look up the path in the assoc list to get the real path to the file.  Then, read that block from the file.
-}
funionRead  :: FSDirectory -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
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


dirStat ctx = FileStat { statEntryType = Directory
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


