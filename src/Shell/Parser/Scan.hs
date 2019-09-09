module Shell.Parser.Scan ( ScanResult(..), scanP, scan1P ) where 

import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as E
import qualified Data.Text                      as T
import qualified Data.Text.Array                as TI
import qualified Data.Text.Internal             as TI
import qualified Data.Text.Internal.Unsafe.Char as TI

import Data.Proxy
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Internal

data ScanResult st = Continue st
                   | Done
                   | Expected Char String
                   | OutOfInput String
                   deriving (Show)

scan_ :: (st -> Maybe Char -> ScanResult st) -> st -> T.Text -> (T.Text, T.Text, ScanResult st)
scan_ p state orig@(TI.Text array offset len) = uglyScan state 0
    where
        -- Mucking around with secret internals is merely *ugly*, as opposed to the unalloyed evil of unsafePerformIO
        uglyScan st off
            | off < len =
                case p st (Just $ TI.unsafeChr (TI.unsafeIndex array (offset + off))) of
                    Continue st' | off + 1 == len -> (TI.Text array offset (off + 1), TI.Text array (offset + off + 1) (len - off - 1), p st' Nothing)
                                 | otherwise      -> uglyScan st' (off + 1)
                    err -> (TI.Text array offset off, TI.Text array (offset + off) (len - off), err)
            | otherwise = (orig, T.empty, p st Nothing)
        
scanP :: forall e m st.
         Maybe String
      -> st
      -> (st -> Maybe Char -> ScanResult st)
      -> ParsecT e T.Text m T.Text
scanP ml st f = ParsecT $ \(State input o pst) cok cerr eok eerr -> do
  let pxy = Proxy :: Proxy T.Text
      (ts, input', st') = scan_ f st input
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  case st' of
    Continue _ -> error "The impossible happened (scan ended while continuing?)"
    Done | chunkEmpty pxy ts -> eok ts (State input' (o + len) pst) hs
         | otherwise         -> cok ts (State input' (o + len) pst) hs
    Expected unexp exp | chunkEmpty pxy ts -> eerr errMsg (State input' (o + len) pst)
                       | otherwise         -> cerr errMsg (State input' (o + len) pst)
        where
            unexpected = Just (Tokens (pure unexp))
            expected = maybe mempty (E.singleton . Label) (NE.nonEmpty exp)
            errMsg = TrivialError (o + len) unexpected expected
    OutOfInput exp | chunkEmpty pxy ts -> eerr errMsg (State input' (o + len) pst)
                   | otherwise         -> cerr errMsg (State input' (o + len) pst)
        where
            unexpected = Just (Label $ NE.fromList "end of input")
            expected = maybe mempty (E.singleton . Label) (NE.nonEmpty exp)
            errMsg = TrivialError (o + len) unexpected expected
{-# INLINE scanP #-}

scan1P :: forall e m st.
        Maybe String
     -> st
     -> (st -> Maybe Char -> ScanResult st)
     -> ParsecT e T.Text m T.Text
scan1P ml st f = ParsecT $ \(State input o pst) cok cerr _ eerr -> do
    let pxy = Proxy :: Proxy T.Text
        (ts, input', st') = scan_ f st input
        len = chunkLength pxy ts
        el = Label <$> (ml >>= NE.nonEmpty)
        hs =
            case el of
                Nothing -> mempty
                Just l -> (Hints . pure . E.singleton) l
    case st' of
        Continue _ -> error "The impossible happened (scan ended while continuing?)"
        Done | chunkEmpty pxy ts -> let 
                    us = pure $
                        case take1_ input of
                            Nothing -> EndOfInput
                            Just (t,_) -> Tokens ((NE.:| []) t)
                    ps    = maybe E.empty E.singleton el
                in eerr (TrivialError o us ps) (State input o pst)
             | otherwise         -> cok ts (State input' (o + len) pst) hs
        Expected unexp exp | chunkEmpty pxy ts -> eerr errMsg (State input' (o + len) pst)
                           | otherwise         -> cerr errMsg (State input' (o + len) pst)
            where
                unexpected = Just (Tokens (pure unexp))
                expected = maybe mempty (E.singleton . Label) (NE.nonEmpty exp)
                errMsg = TrivialError (o + len) unexpected expected
        OutOfInput exp | chunkEmpty pxy ts -> eerr errMsg (State input' (o + len) pst)
                       | otherwise         -> cerr errMsg (State input' (o + len) pst)
            where
                unexpected = Just (Label $ NE.fromList "end of input")
                expected = maybe mempty (E.singleton . Label) (NE.nonEmpty exp)
                errMsg = TrivialError (o + len) unexpected expected
{-# INLINE scan1P #-}
