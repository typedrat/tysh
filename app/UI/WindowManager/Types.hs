module UI.WindowManager.Types
    ( 
    -- * @WindowManagerState@
      WindowManagerState, HasWindowManagerState(..), initialWindowManagerState
    , NameSupply, freshName
    -- * @WindowState@
    , WindowState, wsTitle, wsSize
    , WindowSize, _Maximized, _Minimized, _Sized
    ) where

import           Control.Lens
import           Control.Zipper
import qualified Data.Map           as M
import qualified Data.Text          as T

--

-- | An infinite list of window names.
newtype NameSupply n = NameSupply [n]

freshName :: NameSupply n -> (n, NameSupply n)
freshName (NameSupply (n:ns)) = (n, NameSupply ns)
freshName (NameSupply [])     = error "Name supply exhausted!"

instance (Show n) => Show (NameSupply n) where
    show (NameSupply xs) 
        | null rest = show ys
        | otherwise = show ys ++ "..." 
        where
            (ys, rest) = splitAt 3 xs

--

newtype WindowZipper n = WindowZipper (Top :>> M.Map n WindowState :> (WindowState :@ n))

_WindowZipper :: Iso' (WindowZipper n) (Top :>> M.Map n WindowState :> (WindowState :@ n))
_WindowZipper = iso (\(WindowZipper z) -> z) WindowZipper
{-# INLINE _WindowZipper #-}

instance (Show n) => Show (WindowZipper n) where
    show (WindowZipper z) = show (z ^@. focus)

--

data WindowManagerState n = WindowManagerState 
    { _wmFocusZipper   :: WindowZipper n
    , _wmNames         :: NameSupply n
    }
    deriving (Show)

class HasWindowManagerState c n | c -> n where
    wmState :: Lens' c (WindowManagerState n)

    wmFocusZipper :: Lens' c (Top :>> M.Map n WindowState :> (WindowState :@ n))
    wmFocusZipper = wmState . wmFocusZipper' . _WindowZipper
        where
            wmFocusZipper' = lens _wmFocusZipper (\wm fz -> wm { _wmFocusZipper = fz })
    {-# INLINE wmFocusZipper #-}
    
    wmNames :: Lens' c (NameSupply n)
    wmNames = wmState . wmNames'
        where
            wmNames' = lens _wmNames (\wm ns -> wm { _wmNames = ns })
    {-# INLINE wmNames #-}

instance HasWindowManagerState (WindowManagerState n) n where
    wmState = simple
    {-# INLINE wmState #-}

initialWindowManagerState :: (Ord n) => [n] -> WindowManagerState n
initialWindowManagerState names = WindowManagerState (WindowZipper windowZipper) (NameSupply names')
    where
        (name:names') = names
        windowMap = M.singleton name (WindowState Nothing Maximized)
        -- Nothing would only be returned if the map was empty,
        -- and it will never be empty.
        Just windowZipper = iwithin itraversed (zipper windowMap)

--

data WindowState = WindowState 
    { _wsTitle       :: Maybe T.Text
    , _wsSize        :: WindowSize
    }
    deriving (Show)

wsTitle :: IndexPreservingLens' WindowState (Maybe T.Text)
wsTitle = iplens _wsTitle (\ws t -> ws { _wsTitle = t })
{-# INLINE wsTitle #-}

wsSize :: IndexPreservingLens' WindowState WindowSize
wsSize = iplens _wsSize (\ws sz -> ws { _wsSize = sz })
{-# INLINE wsSize #-}

--

data WindowSize = Maximized | Minimized | Sized (Int, Int)
                deriving (Eq, Show)

trivialPrism :: (Eq a) => a -> Prism' a ()
trivialPrism x = prism' (const x) (\y -> if x == y then Just () else Nothing)
{-# INLINE trivialPrism #-}

_Maximized :: Prism' WindowSize ()
_Maximized = trivialPrism Maximized
{-# INLINE _Maximized #-}

_Minimized :: Prism' WindowSize ()
_Minimized = trivialPrism Minimized
{-# INLINE _Minimized #-}

_Sized :: Prism' WindowSize (Int, Int)
_Sized = prism' Sized $ \ws -> case ws of
            Sized hw -> Just hw
            _        -> Nothing
{-# INLINE _Sized #-}
