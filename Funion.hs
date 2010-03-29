module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse
import System(getArgs)
import System.Environment(withArgs)

import DirTools.DirTools

type HT = ()

{-TODO(nathan):

* Need to wire read and open to real directories.
* Need to get the stats of a file/dir on the real FS
* Need to refactor.  A LOT.


OPEN QUESTIONS:
For now, I suppose that I could just specify the directories that I want to union on the command line.
What about making this more aggressive and having some way of specifying unioning rules?  Perhaps
via a DSL?
-}


main :: IO ()
main = do
  args <- getArgs
  let args' = map (\(x:xs)-> xs) $ filter (\(x:xs) -> if x == '+' then True else False) args
  let args2 = filter(\(x:xs) -> if x == '+' then False else True) args
  putStrLn $ show args'
  dirs <- mapM (`readDir` "") $ tail args'
  let tree = treeUnion "" dirs
  withArgs args2 $ fuseMain (funionFSOps tree)  defaultExceptionHandler

myMessage :: B.ByteString
myMessage = B.pack "Hello there world.  This is a test of the nathan broadcast system\n"

funionFSOps :: FSDirectory -> FuseOperations HT
funionFSOps dir@(FSDirectory _ _ _) = 
  defaultFuseOps{ fuseGetFileStat = funionGetFileStat dir
                , fuseOpen        = funionOpen dir
                , fuseRead        = funionRead dir
                , fuseOpenDirectory = funionOpenDirectory dir
                , fuseReadDirectory = funionReadDirectory dir
                , fuseGetFileSystemStats = funionGetFileSystemStats dir
                }


funionGetFileStat :: FSDirectory -> FilePath -> IO (Either Errno FileStat)
funionGetFileStat fileTree@(FSDirectory _ _ _) "/" = do 
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

{- TODO(nathan):
    Have this work on the files from the unioned filesystem instead.   Look up the path names in the assoc. list
-}
funionGetFileStat tree  path = do -- | path == "/afile" || path == "/afile1" || path == "/afile2" || path == "/afile3" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
--funionGetFileStat  _ _ = 
 --   return $ Left eNOENT


{- TODO(nathan):
    Look up paths in the assoc list.  If exist then return (Right ()) else Left e*)
-}
funionOpen :: FSDirectory -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
funionOpen tree path mode flags
    | (path == "/afile") || (path == "/afile1") ||  (path == "/afile2") || (path == "/afile3") = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise       = return (Left eNOENT) 


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


funionOpenDirectory :: FSDirectory -> FilePath -> IO Errno
funionOpenDirectory fileTree "/" =  return eOK
funionOpenDirectory fileTree _ = return eNOENT

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
{-
    Have this generated from the assoc. list.
    Will need to make it work with sub-directories too.
-}
funionReadDirectory fileTree "/" = do
    ctx <- getFuseContext
    return $ Right $[ (".", dirStat ctx)
                    ,("aDIR",dirStat' ctx),("..", dirStat ctx)] 
                ++  (map (\x-> ((fsEntryFileName $ dirStats x), dirStat ctx)) (dirDirs fileTree))
--                ++  (map (\x-> (fsEntryFileName $ fileStats x, fileStat ctx)) (dirFiles fileTree))

funionReadDirectory fileTree _ = return (Left (eNOENT))

{-
    The following two should be completely removed and replaced with
    functions which wrap the statistics of the real files and directories
    in FileStats
-}
dirStat' ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = 1000
                       , statFileGroup = 1000
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }


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


