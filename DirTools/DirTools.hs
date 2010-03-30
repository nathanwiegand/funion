module DirTools.DirTools
  ( prettyTree
  , treeUnion
  , readDir
  , FSDirectory (..)
  , FSEntryStatistics (..)
  , fileStats
  , getSubTree
  )
where

import System.Directory
import System.Time
import System.FilePath.Posix
import Data.List
import Data.Maybe
import Control.Monad

{-
  TODO(nathan): right now the function that tries to fix name conflicts may 
  actually result in other conflicts.  Ooops.
-}    

data FSEntryStatistics = FSEntryStatistics {
    fsEntryFileName    :: String
  , fsEntryActualPath  :: FilePath
  , fsEntryVirtualPath :: FilePath
  , fsEntryPermissions :: Permissions
  , fsEntryModificationTime :: ClockTime
  }
  deriving (Show)

data File = File {
    fileStats :: FSEntryStatistics
  }
  deriving (Show)

data FSDirectory = FSDirectory {
    dirStats :: FSEntryStatistics
  , dirFiles :: [File]
  , dirDirs  :: [FSDirectory]
  }
  deriving (Show)

treeUnion :: FilePath -> [FSDirectory] -> FSDirectory 
treeUnion path [] = undefined
treeUnion path dirs = 
  FSDirectory (FSEntryStatistics {
      fsEntryFileName = fsEntryFileName newDirStatistics
    , fsEntryActualPath = ""
    , fsEntryVirtualPath = newPath
    , fsEntryPermissions = r_x
    , fsEntryModificationTime = TOD 1234567890 0 -- need to improve
    }) 
    (unionFiles $ sortByFileName $ concat $ map (dirFiles) dirs)
    (unionDirs  $ sortByDirName $ concat $ map (dirDirs) dirs)
  where 
    newDirStatistics = dirStats $ head dirs
    newPath = path ++ "/" ++(fsEntryFileName newDirStatistics)

    sortByFileName  = sortBy (\a b -> compare (fsEntryFileName $ fileStats a) (fsEntryFileName $ fileStats b)) 

    sameFileName :: File -> File -> Bool
    sameFileName (File stat1) (File stat2) = (fsEntryFileName stat1) == (fsEntryFileName stat2)

    unionFiles :: [File] -> [File]
    unionFiles files =  concat $ map (renameFiles 0) $ groupBy (sameFileName) files

    renameFiles :: Int -> [File] -> [File]
    renameFiles n [x] = [updateFileName n x]
    renameFiles n (x:xs) = (updateFileName n x) : (renameFiles (n+1) xs)

    updateFileName :: Int  -> File -> File
    updateFileName n (File (FSEntryStatistics name actpath virtpath perm time)) = File (FSEntryStatistics {
        fsEntryFileName = name ++ ext 
      , fsEntryActualPath = actpath
      , fsEntryVirtualPath =newPath ++ "/" ++ name ++ ext
      , fsEntryPermissions = perm
      , fsEntryModificationTime =  time
      })
      where ext = if n == 0 then "" else "~" ++ show n

    sortByDirName  = sortBy (\a b -> compare (fsEntryFileName $ dirStats a) (fsEntryFileName $ dirStats b)) 

    sameDirName :: FSDirectory -> FSDirectory -> Bool
    sameDirName (FSDirectory stat1 _ _) (FSDirectory stat2 _ _) = (fsEntryFileName stat1) == (fsEntryFileName stat2)

    unionDirs :: [FSDirectory] -> [FSDirectory]
    unionDirs dirs =  map (treeUnion (newPath)) $ groupBy (sameDirName) dirs


r_x = Permissions { readable = True, writable = False, executable = True, searchable = True}

prettyTree tree = prettyTree' 0 [tree]
prettyTree' tabStop treeLevel = concat $ map (prettyEntry tabStop) treeLevel

prettyStats tabStop stats = (concat $ replicate tabStop "  ") ++  
    fsEntryFileName stats ++ "    " ++ fsEntryVirtualPath stats ++ "     " ++ 
    fsEntryActualPath stats ++ "\n" 

prettyEntry :: Int -> FSDirectory -> String
prettyEntry tabStop (FSDirectory stats files dirs) = "*" ++ 
    (prettyStats tabStop stats) ++ 
    (concat $ map ((prettyStats $ tabStop+1) . fileStats) files) ++ 
    (concat $ map (prettyEntry $ tabStop+1) dirs)

dirContents :: FilePath -> IO [FilePath]
dirContents = fmap (filter (`notElem` [".",".."])) . getDirectoryContents 

dirExists :: FilePath -> String -> IO Bool
dirExists path name = doesDirectoryExist (path ++ "/" ++ name)

fileExists :: FilePath -> String -> IO Bool
fileExists path name = doesFileExist (path ++ "/" ++ name)

getStats :: FilePath -> String -> IO FSEntryStatistics
getStats path name = do
  let uri = path ++ "/" ++  name
  perm <- getPermissions uri
  time <- getModificationTime uri
  return FSEntryStatistics {
    fsEntryFileName = name
  , fsEntryActualPath = uri
  , fsEntryVirtualPath = ""
  , fsEntryPermissions = perm
  , fsEntryModificationTime = time
  }



getSubTree :: FilePath ->  FSDirectory ->  FSDirectory
getSubTree path dir = if length pathParts > 0 then  getSubTree (joinPath $ tail pathParts) subtree else dir
  where 
    pathParts = splitDirectories path
    subtree =  fromJust $ find (\x -> (head pathParts) == (fsEntryFileName $ dirStats x)) (dirDirs dir) 
--    subtree = case find (\x -> (head pathParts) == (fsEntryFileName $ dirStats x)) (dirDirs dir) of
 --                 Just x -> x
  --                _      -> dir




readDir :: FilePath -> String -> IO FSDirectory
readDir path name = do
  --let uri = path ++ "/" ++ name
  {- replace with joinPath -}
  let uri = if length name > 0 then path ++ "/" ++ name else path 
  perm <- getPermissions uri 
  time <- getModificationTime uri 
  contents <- dirContents uri
  --files <- filterM (doesFileExist) $ map (uri++) contents
  files <- filterM (fileExists uri) contents
  fileList <- mapM (getStats (uri)) files

  -- list of directories
  dirs <- filterM (dirExists uri) contents
  dirtree <- mapM (readDir uri) dirs
  
  return $ FSDirectory (FSEntryStatistics {
      fsEntryFileName = name
    , fsEntryActualPath = path ++ name
    , fsEntryVirtualPath = ""
    , fsEntryPermissions = perm
    , fsEntryModificationTime = time
    })  (map (File) fileList)
        dirtree
  


main = do
  dir <- readDir "/dvds" ""
  dir' <- readDir "/disk2/dvds" ""
  let dir2 = getSubTree "Torchwood/Season 1" dir
--  putStrLn $ prettyTree dir2 -- $ treeUnion "" [dir,dir']
  putStrLn $ prettyTree  $ treeUnion "" [dir,dir']

