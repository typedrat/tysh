{-# OPTIONS_GHC -Wno-orphans #-}
module Shell.Evaluator.Spawn 
    ( FileActions(..)
    , spawn, spawnp
    ) where

import Control.Monad.Cont
import Data.Bits ( (.|.) )
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.ContT
import Foreign.Storable
import System.Posix.IO  ( OpenMode(..), OpenFileFlags(..) )
import System.Posix.Types

#include <HsUnix.h>
#include <spawn.h>

data CSpawnAttrs

nullSpawnAttrs :: Ptr CSpawnAttrs
nullSpawnAttrs = nullPtr

data CFileActions

withCFileActions :: ContT r IO (Ptr CFileActions)
withCFileActions = bracketContT cFileActionsInit
                                cFileActionsDestroy
                                (allocaBytes #{size posix_spawn_file_actions_t})

foreign import ccall "spawn.h posix_spawn_file_actions_init"
    faInitPtr :: Ptr CFileActions -> IO CInt
foreign import ccall "spawn.h posix_spawn_file_actions_addopen"
    faOpen :: Ptr CFileActions -> Fd -> CString -> CInt -> CMode -> IO CInt
foreign import ccall "spawn.h posix_spawn_file_actions_addclose"
    faClose :: Ptr CFileActions -> Fd -> IO CInt
foreign import ccall "spawn.h posix_spawn_file_actions_adddup2"
    faDup2 :: Ptr CFileActions -> Fd -> Fd -> IO CInt
foreign import ccall "spawn.h posix_spawn_file_actions_destroy"
    faDestroyPtr :: Ptr CFileActions -> IO CInt

cFileActionsInit :: Ptr CFileActions -> IO ()
cFileActionsInit ptr =
    throwErrnoIfMinus1_ "cFileActionsInit" $ faInitPtr ptr

cFileActionsDestroy :: Ptr CFileActions -> IO ()
cFileActionsDestroy ptr =
    throwErrnoIfMinus1_ "cFileActionsDestroy" $ faDestroyPtr ptr

cFileActionsOpen :: Ptr CFileActions -> Fd -> CString -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO ()
cFileActionsOpen ptr fd path how maybe_mode OpenFileFlags{..} =
        throwErrnoIfMinus1_ "cFileActionsOpen" $ faOpen ptr fd path all_flags mode_w
    where
        all_flags  = creat .|. flags .|. open_mode
    
        flags =
            (if append    then (#const O_APPEND)   else 0) .|.
            (if exclusive then (#const O_EXCL)     else 0) .|.
            (if noctty    then (#const O_NOCTTY)   else 0) .|.
            (if nonBlock  then (#const O_NONBLOCK) else 0) .|.
            (if trunc     then (#const O_TRUNC)    else 0)
         
        (creat, mode_w) = case maybe_mode of
                            Nothing -> (0,0)
                            Just x  -> ((#const O_CREAT), x)
    
        open_mode = case how of
                       ReadOnly  -> (#const O_RDONLY)
                       WriteOnly -> (#const O_WRONLY)
                       ReadWrite -> (#const O_RDWR)

cFileActionsClose :: Ptr CFileActions -> Fd -> IO ()
cFileActionsClose ptr fd =
    throwErrnoIfMinus1_ "cFileActionsClose" $ faClose ptr fd

cFileActionsDup2 :: Ptr CFileActions -> Fd -> Fd -> IO ()
cFileActionsDup2 ptr fd1 fd2 =
    throwErrnoIfMinus1_ "cFileActionsDup2" $ faDup2 ptr fd1 fd2

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

foreign import ccall "spawn.h posix_spawn"
    posix_spawn :: Ptr ProcessID -> CString -> Ptr CFileActions -> Ptr CSpawnAttrs
                -> Ptr CString -> Ptr CString -> IO CInt 
foreign import ccall "spawn.h posix_spawnp"
    posix_spawnp :: Ptr ProcessID -> CString -> Ptr CFileActions -> Ptr CSpawnAttrs
                 -> Ptr CString -> Ptr CString -> IO CInt

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
