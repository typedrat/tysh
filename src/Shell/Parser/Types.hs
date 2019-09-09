module Shell.Parser.Types
    ( RedirectMode(..)
    , RedirectTarget(..)
    , Redirection(..)
    , Atom(..)
    , Syntax(..)
    , Parser
    ) where

import qualified Data.Text as T
import Data.Void
import Text.Megaparsec ( Parsec )
import System.Posix.Types ( Fd )

data RedirectMode = Read | Overwrite | Append
                  deriving (Show, Eq)

data RedirectTarget = ToFile Atom
                    | ToFd Fd
                    | CloseFd
                    deriving (Show, Eq)

data Redirection = Redirection { redirectFd :: Fd
                               , redirectMode :: RedirectMode
                               , redirectTarget :: RedirectTarget
                               }
                 deriving (Show, Eq)

data Atom = Bare T.Text
          | SinglyQuoted T.Text
          | DoublyQuoted [Atom]
          | Variable T.Text
          deriving (Show, Eq)

data Syntax = Command { cmdExe :: Atom
                      , cmdArgs :: [Atom]
                      , cmdRedirections :: [Redirection]
                      , cmdIsBackground :: Bool
                      }
            | Pipe Syntax Syntax
            | AndOp Syntax Syntax
            | OrOp Syntax Syntax

            deriving (Show, Eq)

type Parser = Parsec Void T.Text
