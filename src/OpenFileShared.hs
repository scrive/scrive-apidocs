{-# LANGUAGE CPP #-}
module OpenFileShared where

import qualified System.Posix.IO as Posix
import qualified System.Posix.Internals as Posix
import System.IO
import Foreign.C.Types
import GHC.IO.FD
import GHC.IO.Device as IODevice
import GHC.IO.Handle.FD

-- | Make a 'FD' from an existing file descriptor.  Fails if the FD
-- refers to a directory.  If the FD refers to a file, `mkFD` locks
-- the file according to the Haskell 98 single writer/multiple reader
-- locking semantics (this is why we need the `IOMode` argument too).
mkFDNoLock :: CInt
           -> IOMode
           -> Bool   -- ^ is a socket (on Windows)
           -> Bool   -- ^ is in non-blocking mode on Unix
           -> IO FD

mkFDNoLock fd _iomode is_socket is_nonblock = do

    let _ = (is_socket, is_nonblock) -- warning suppression

#ifdef mingw32_HOST_OS
    _ <- setmode fd True -- unconditionally set binary mode
#endif

    return (FD{ fdFD = fd,
#ifndef mingw32_HOST_OS
                fdIsNonBlocking = fromEnum is_nonblock
#else
                fdIsSocket_ = fromEnum is_socket
#endif
              })

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt
#endif



-- | Turn an existing file descriptor into a Handle.  This is used by
-- various external libraries to make Handles.
--
-- Makes a binary Handle.  This is for historical reasons; it should
-- probably be a text Handle with the default encoding and newline
-- translation instead.
fdToHandleNoLock :: Posix.FD -> IO Handle
fdToHandleNoLock fdint = do
   iomode <- Posix.fdGetMode fdint
   fd <- mkFDNoLock fdint iomode
            False{-is_socket-}
              -- NB. the is_socket flag is False, meaning that:
              --  on Windows we're guessing this is not a socket (XXX)
            True{-is_nonblock-}
   let fd_str = "<file descriptor: " ++ show fd ++ ">"
   mkHandleFromFD fd RegularFile fd_str iomode True{-non-block-}
                  Nothing -- bin mode


openFileShared :: FilePath -> IOMode -> IO Handle
openFileShared filePath iomode = do
    let posixMode = case iomode of
                      ReadMode      -> Posix.ReadOnly
                      WriteMode     -> Posix.WriteOnly
                      AppendMode    -> Posix.WriteOnly
                      ReadWriteMode -> Posix.ReadWrite
        fileFlags = Posix.defaultFileFlags { Posix.append = iomode == AppendMode
                                           , Posix.trunc  = iomode == WriteMode
                                           , Posix.nonBlock = True
                                           }
    fd <- Posix.openFd filePath posixMode (Just (6*8*8 + 6*8 + 6)) fileFlags
    fdToHandleNoLock (fromIntegral fd)
