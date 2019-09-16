module Shell.Evaluator.Spawn.CBits 
    ( CSpawnAttrs, nullSpawnAttrs
    , CFileActions, withCFileActions 
    , cFileActionsInit, cFileActionsDestroy
    , cFileActionsOpen, cFileActionsDup2, cFileActionsClose
    , posix_spawn, posix_spawnp
    ) where

import Data.Bits ( (.|.) )
import Foreign.C.Error ( throwErrnoIfMinus1_ )
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.ContT ( ContT, allocaBytes, bracketContT, Ptr, nullPtr )
import System.Posix.IO  ( OpenMode(..), OpenFileFlags(..) )
import System.Posix.Types ( CMode(..), Fd(..), FileMode, ProcessID )

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

cFileActionsOpen :: Ptr CFileActions -> Fd -> CString
                 -> OpenMode -> Maybe FileMode -> OpenFileFlags
                 -> IO ()
cFileActionsOpen ptr fd path how maybe_mode OpenFileFlags{..} =
        throwErrnoIfMinus1_ "cFileActionsOpen" $
            faOpen ptr fd path all_flags mode_w
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

foreign import ccall "spawn.h posix_spawn"
    posix_spawn :: Ptr ProcessID -> CString -> Ptr CFileActions
                -> Ptr CSpawnAttrs -> Ptr CString -> Ptr CString
                -> IO CInt
foreign import ccall "spawn.h posix_spawnp"
    posix_spawnp :: Ptr ProcessID -> CString -> Ptr CFileActions
                 -> Ptr CSpawnAttrs -> Ptr CString -> Ptr CString
                 -> IO CInt
