module KMonad.Keyboard.Linux

where

import KMonad.Prelude
import KMonad.Keyboard.Linux.Types
import KMonad.Keyboard.Linux.Uinput

data LinuxEnv = LinuxEnv
  { _uinput  :: UinputEnv
  , _evdev   :: ()
  , _logFunc :: LogFunc
  }
makeClassy ''LinuxEnv

instance HasUinputEnv LinuxEnv where uinputEnv = uinput
instance HasUinputCfg LinuxEnv where uinputCfg = uinput.uinputCfg
instance HasLogFunc   LinuxEnv where logFuncL  = logFunc

mkLinuxEnv :: HasLogFunc e => UinputCfg -> RIO e LinuxEnv
mkLinuxEnv ucfg = do
  lf   <- view logFuncL
  uenv <- mkUinputEnv ucfg
  pure $ LinuxEnv uenv () lf

type Linux = RIO LinuxEnv

instance CanOS Linux where
  type Keycode Linux = LC

  getKey = undefined
  putKey = uinputWrite
  runOS  = bracket_ uinputOpen uinputClose

