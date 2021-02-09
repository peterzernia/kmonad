{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.Types
Description : Basic keyboard types
Copyright   : (c) David Janssen, 2020
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The following module contains all the basic, OS-agnostic types that we use to
reason about keyboards, their elements, and their events. Note that some of the
terminology can be ambiguous, so for clarity's sake this is how we use
terminology in KMonad:

A keyboard is a collection of different entities that can exist as either
pressed or released.

When we say *key* we are using this to refer to the unique identifiability of an
element (by its 'Keycode').

When we say *button* we are using this to refer to the fact that the elements of
a keyboard have 2 states, and that changing between these states causes actions
to occur.

Buttons are bound to keys, and when a state-change associated with a particular
key is detected ('KeyEvent'), that key's button is looked up, and it's state is
similarly changed.

-}
module KMonad.Keyboard.Types
  ( -- * Keycode
    -- $code
    Keyname
  , Keycode
  , HasKeycode(..)
  , mkKeycode

    -- * Switch
    -- $switch
  , Switch(..)
  , HasSwitch(..)
  , AsSwitch(..)
  , _Pressed

    -- * KeySwitch
    -- $keyswitch
  , KeySwitch
  , HasKeySwitch(..)
  )
where

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $code
--
-- Here we define the basic 'Keycode' type. How to do this exactly is a little
-- bit tricky, since every OS represents their 'Keycode's very differently. In
-- the end we simply represent 'Keycode's as a newtype around 'Int'. How each OS
-- maps into and out of the KMonad representation is defined for each OS
-- seperately in the "KMonad.OS" modules, but anything that can 'Enum' can work
-- as a 'Keycode'

-- | 'Keyname' is used to refer to keycodes, we simply use text
type Keyname = Text

-- | 'Keycode's are values that can be compared for equality, nothing more
newtype Keycode = Keycode Int deriving (Eq, Show)

-- | Take some 'Enum' value and turn it into a Keycode
mkKeycode :: Enum a => a -> Keycode
mkKeycode = Keycode . fromEnum

-- | A class describing how to access some value's 'Keycode'.
class HasKeycode a where
  keycode :: Lens' a Keycode

-- | Hooks the class specific definition of 'Keycode' into 'HasKeycode'
instance HasKeycode Keycode where
  keycode = id

--------------------------------------------------------------------------------
-- $switch
--
-- 'Switch' describes a state-transition for a 2-state system, with the
-- additional semantics of some sort of /enabled/ vs. /disabled/ context.
-- Basically this is just a 'Bool', but with some extra clarity

-- | An ADT describing all the state-changes a button or key can undergo.
data Switch
  = Press   -- ^ Change from disabled to enabled
  | Release -- ^ Change from enabled to disabled
  deriving (Eq, Ord, Enum, Show, Generic, Hashable)
makeClassyPrisms ''Switch


-- | A class describing how to access some value's 'Switch'
class HasSwitch a where
  switch :: Lens' a Switch

-- | An 'Iso' between 'Bool' and 'Switch' where 'Press' evaluates to 'True'
_Pressed :: Iso' Switch Bool
_Pressed = iso (Press ==) (bool Release Press)

-- | Hooks the base 'Switch' type into 'HasSwitch'
instance HasSwitch Switch where switch = id

-- | How to pretty-print a 'Switch'
instance Display Switch where
  textDisplay = tshow

--------------------------------------------------------------------------------
-- $keyswitch
--
-- A 'KeySwitch' is a detected state-change for some key, identified by its
-- 'Keycode'.

-- | The 'KeySwitch' record, containing a 'Switch' and an identifying 'Keycode'
data KeySwitch = KeySwitch
  { _ksSwitch :: !Switch  -- ^ wether a 'press' or 'release' occurred
  , _ksCode   :: !Keycode -- ^ the identity of the key which registered the event
  } deriving (Eq, Show)
makeLenses ''KeySwitch

class HasKeySwitch a where
  keySwitch :: Lens' a KeySwitch

instance HasKeySwitch KeySwitch where keySwitch = id       -- ^ Hook into HasKeySwitch
instance HasSwitch    KeySwitch where switch    = ksSwitch -- ^ Hook into HasSwitch
instance HasKeycode   KeySwitch where keycode   = ksCode   -- ^ Hook into HasKeycode

