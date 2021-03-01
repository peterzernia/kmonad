{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, FlexibleContexts #-}
module KMonad.Keyboard.OS

where

import KMonad.Prelude



-- | Class describing Key IO operations
class (Eq (Keycode e)) => KeyIO e where
  -- | The type used as 'Keycode' for this implementation
  type Keycode e :: *
  -- | How to await the next event from the OS
  getKey :: RIO e (Event e)
  -- | How to emit an event to the OS
  putKey :: Event e -> RIO e ()
  -- | How to acquire and cleanup the environment
  withKIO :: (e -> a) -> IO a

-- | Event with a keycode and switch-direction
data Event a = Event
  { isPress :: Bool
  , keycode :: Keycode a
  }
makeLenses ''Event

-- withKeyIO :: KeyIO kio => (kio -> IO a) -> IO a
-- withKeyIO = undefined


-- data Env kio = Env
--   { stuff :: Int
--   , keyIO :: kio
--   }

-- data TestIO = TestIO
--   { someState :: ()
--   , moreState :: Int
--   } deriving (Eq, Show)
-- makeLenses ''TestIO


-- instance KeyIO TestIO where
--   type Keycode TestIO = Char

--   getKey = undefined
--   putKey = undefined
