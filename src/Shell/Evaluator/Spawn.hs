{-# OPTIONS_GHC -Wno-orphans #-}
module Shell.Evaluator.Spawn 
    ( FileActions(..)
    , spawn, spawnp
    ) where

import Control.Monad.Cont
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Foreign.C.Error
import Foreign.Marshal.ContT
import Foreign.Storable
import System.Posix.IO  ( OpenMode(..), OpenFileFlags(..) )
import System.Posix.Types

import Shell.Evaluator.Spawn.CBits

data FileActions = OpenFile { openPath :: FilePath 
                            , openFd :: Fd
                            , openMode :: OpenMode
                            , openCreatMode :: Maybe FileMode
                            , openFlags :: OpenFileFlags
                            }
                 | CloseFile { closeFd :: Fd }
                 | Dup2 { dup2OldFd :: Fd   
                        , dup2NewFd :: Fd
                        }
                 deriving (Show, Eq)
deriving instance Show OpenMode
deriving instance Show OpenFileFlags
deriving instance Eq OpenMode
deriving instance Eq OpenFileFlags

withFileActions :: [FileActions] -> ContT r IO (Ptr CFileActions)
withFileActions [] = return nullPtr
withFileActions xs = do
    ptr <- withCFileActions
    mapM_ (go ptr) xs
    return ptr
    where
        go actions OpenFile{..}   = do
            cPath <- withCString openPath 
            liftIO $ cFileActionsOpen actions openFd cPath openMode openCreatMode openFlags
        go actions CloseFile{..}  = liftIO $ cFileActionsClose actions closeFd
        go actions Dup2{..}       = liftIO $ cFileActionsDup2 actions dup2OldFd dup2NewFd

type Argv = [BS.ByteString]
type Env  = M.Map BS.ByteString BS.ByteString

allocStringArrayWith :: [BS.ByteString] -> ContT r IO (Ptr CString)
allocStringArrayWith xs = allocaArrayWith0' withCString xs nullPtr

allocStringMapWith :: M.Map BS.ByteString BS.ByteString -> ContT r IO (Ptr CString)
allocStringMapWith xs = iallocaArrayWith0' (\k v -> withCString (k <> "=" <> v)) xs nullPtr

spawn :: FilePath -> [FileActions] -> Argv -> Env -> IO ProcessID
spawn path actions argv env = flip runContT return $ do
    cPath <- withCString path
    cActions <- withFileActions actions
    cArgv <- allocStringArrayWith argv
    cEnv <- allocStringMapWith env
    pidPtr <- alloca
    liftIO . throwErrnoIf_ (/= 0) path $ 
        posix_spawn pidPtr cPath cActions nullSpawnAttrs cArgv cEnv
    liftIO $ peek pidPtr

spawnp :: String -> [FileActions] -> Argv -> Env -> IO ProcessID
spawnp command actions argv env = flip runContT return $ do
    cPath <- withCString command
    cActions <- withFileActions actions
    cArgv <- allocStringArrayWith argv
    cEnv <- allocStringMapWith env
    pidPtr <- alloca
    liftIO . throwErrnoIf_ (/= 0) command $ 
        posix_spawnp pidPtr cPath cActions nullSpawnAttrs cArgv cEnv
    liftIO $ peek pidPtr
