{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

------------------ E X T E R N A L
import Data.Function ((&))
import Control.Category ((>>>))
import qualified Data.Map as Map
import Control.Category ((>>>))
import Data.Monoid ((<>))
import qualified System.Exit as Exit

------------------ X M O N A D
import Graphics.X11.Types -- for xK_ keys
import qualified Graphics.X11.ExtraTypes.XF86 as XF86

import qualified XMonad as M
import XMonad ((.|.), Full(..), Mirror(..), Tall(..), (|||))
import qualified XMonad.StackSet as StackSet
-- import qualified XMonad.Util.Run as Run

------------------ X M O N A D - C O N T R I B
import qualified XMonad.Hooks.DynamicLog as DL

import qualified XMonad.Layout.Decoration as Deco
import qualified XMonad.Layout.Gaps as Gaps
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.NoFrillsDecoration as NoFrillsDecoration
import qualified XMonad.Layout.PerWorkspace as PerWS
import qualified XMonad.Layout.Renamed as Renamed

import qualified XMonad.Actions.CycleWS as CycleWS
import qualified XMonad.Actions.Submap as Submap

------------------- M I N E
import XMonad.Rooms (Room(..))
import qualified XMonad.Rooms as Rooms
import qualified XMonad.Trash as Trash
import XMonad.MiscTools (numPadKeys, (+++), toEnglish)

import qualified XMonad.My.Theme as Theme
import qualified XMonad.My.Variables as My


--
-------------------------------------------------------
-- M A I N - B A R - C O N F I G
-------------------------------------------------------

main :: IO ()
main =
  myStatusBar My.bottom_xmobarrc myConfig >>=
  myStatusBar My.top_xmobarrc >>=
  M.xmonad

myStatusBar xmobarrc config =
    DL.statusBar
      command
      Theme.logger
      toggleStrutsKey
      config
  where
    command = "xmobar " ++ xmobarrc
    toggleStrutsKey config =
      let mod = (M.modMask config) in
        (mod, xK_n)

myMask = mod4Mask

myConfig = Theme.mainConfig M.def
      { M.terminal = My.term
      , M.layoutHook = myLayouts
      ---- ^ The available layouts
      -- manageHook :: !ManageHook
      ---- ^ The action to run when a new window is opened
      -- handleEventHook :: !(Event -> X All)
      ---- ^ Handle an X event, returns (All True) if the default handler
      ---- should also be run afterwards. mappend should be used for
      ---- combining event hooks in most cases.
      , M.workspaces = myWorkspaces
      , M.modMask = myMask
      , M.keys = \config ->
          Map.union
            (myRoomKeys config)
            (myKeys config)
      ---- ^ The key binding: a map from key presses and actions
      , M.mouseBindings = myMouse
      ---- ^ The mouse bindings
      -- logHook :: !(X ())
      ---- ^ The action to perform when the windows set is changed
      -- , startupHook = M.spawn myAutoStart
      ---- ^ The action to perform on startup
      , M.focusFollowsMouse = False
      , M.clickJustFocuses = False
      -- clientMask :: !EventMask
      ---- ^ The client events that xmonad is interested in
      -- rootMask :: !EventMask
      ---- ^ The root events that xmonad is interested in handleExtraArgs
      }

--
-------------------------------------------------------
-- R O O M S
-------------------------------------------------------

myWorkspaces = Rooms.roomNames myRooms
myRoomKeys config = Rooms.roomsKeyMap_goto mod myRooms
  where mod = M.modMask config

webRooms
  = Room "WEB-UNIV" (Just xK_w)
    (M.spawn $ My.browser "univ")
  : Room "WEB-READ" (Just xK_e)
    (M.spawn $ My.browser "read")
  : Room "WEB-DEV" (Just xK_z)
    (M.spawn $ My.browser "dev")
  : []

videoRoom = Room "VLC" (Just xK_v) action
  where
    action = do
      M.spawn $ My.term_ My.videos

trashRoom = Room "TRASH" (Just xK_x) (pure ())

myRooms :: [Room]
myRooms = simpleRooms
    ++ Room "SOUND" (Just xK_s)
      (M.spawn $ My.music)
    : Room "TERMINAL" (Just xK_t)
      (M.spawn My.term >> M.spawn My.term)
    : videoRoom
    : Room "DEV" (Just xK_d)
      (M.spawn (My.term_ My.dev))
    : Room "CHECK" (Just xK_c)
      (M.spawn $ My.systemView)
    : Room "GIMP" (Just xK_g)
      (M.spawn "gimp")
    : Room "BOX" (Just xK_b)
      (M.spawn $ My.vm)
    : Room "READ" (Just xK_r)
      (M.spawn $ My.term_ My.fictions)
    : trashRoom
    : webRooms
  where
    simpleRooms = Rooms.simpleRooms'
       (fmap toEnglish [1..3])
       [xK_ampersand, xK_eacute, xK_quotedbl]

--
-------------------------------------------------------
-- L A Y O U T S
-------------------------------------------------------

myLayouts =
    PerWS.onWorkspace
      (roomName videoRoom) (Full ||| unnamed decorated tall)
    $ PerWS.onWorkspaces
       (fmap roomName webRooms) (Full ||| tiledLayouts)
    $ (tiledLayouts ||| Full)
  where
    gap = 6
    decorated =
      Spacing.spacing gap >>> -- inbetween windows
      withTopBar
    tiledLayouts = unnamed decorated tall
      ||| unnamed decorated (Mirror tall)
    tall = Tall 1 (3/100) (54/100)

named name = Renamed.renamed [(Renamed.Replace name)]
unnamed modifier layout =
  named (M.description layout) (modifier layout)

withTopBar = NoFrillsDecoration.noFrillsDeco
    Deco.shrinkText
    Theme.topBar

--
-------------------------------------------------------
-- K E Y M A P
-------------------------------------------------------

myKeys conf = Map.fromList
  ---- ORIGINAL BINDINGS
    $ (mod,         xK_p)            +++ M.spawn My.pmenu
    : (mod,         xK_l)            +++ M.sendMessage M.NextLayout
    : (modshift,    xK_l)            +++ M.setLayout (M.layoutHook conf)
                                      -- ^ resetLayout
    : (mod,         xK_Tab)  
                +++ Rooms.initRoomElse myRooms (M.windows StackSet.focusDown)
                -- ^ if current ws is empty and is a room in first arg,
                -- ^ launch this room's action
                -- ^ otherwise, execute the second argument
    : (modshift,    xK_Tab)          +++ M.windows StackSet.focusUp
    : (mod,         xK_Return)       +++ M.windows StackSet.swapMaster
    : (mod,         xK_q)            +++ restartXMonad
    : (modshift,    xK_q)            +++ withSecurity xK_q 3 quitXMonad
    : (mod,         xK_m)            +++ M.refresh

  ---- usual DE bindings
    : (alt,         xK_F3)           +++ M.spawn My.allApps
    : (0,           xK_Print)        +++ M.spawn My.screenshooter
    : (ctrlAlt,     xK_Escape)       +++ M.spawn My.escape

  ---- cmus
    : (0,     XF86.xF86XK_AudioPrev)     +++ M.spawn My.musicPrev
    : (0,     XF86.xF86XK_AudioNext)     +++ M.spawn My.musicNext
    : (0,     XF86.xF86XK_AudioPlay)     +++ M.spawn My.musicPlayPause

  ---- WIN ACTIONS
    : (mod,         xK_k)            +++
             Trash.trashWS (Rooms.roomName trashRoom)

  ---- SPAWN
    : (ctrlAlt,     xK_g)            +++ M.spawn My.switchUser
    : (ctrlAlt,     xK_t)            +++ M.spawn My.term
    : (ctrlAlt,     xK_f)            +++ M.spawn My.guiFileManager
    : (ctrlAlt,     xK_w)            +++ M.spawn My.browserProfileManager

  ---- ROOMS SUBMAP
    : (mod,         xK_Pause)        +++ myHallway
    : (0,           xK_Pause)        +++ myHallway

  ---- LAYOUT
    : (mod,         xK_semicolon)    +++ M.sendMessage M.Shrink
    : (mod,         xK_colon)        +++ M.sendMessage M.Expand
  ---- MOVE WS
    : (mod,         xK_space)        +++ CycleWS.toggleWS
    : (mod,         xK_Left)         +++ CycleWS.prevWS
    : (mod,         xK_Right)        +++ CycleWS.nextWS
    : (mod,         xK_a)        +++ CycleWS.nextWS
    : keypadWorkspaces
  where
    mod = M.modMask conf
    ctrlAlt = controlMask .|. alt
    alt = mod1Mask
    modshift = mod .|. shiftMask
    ws = M.workspaces conf

    restartXMonad = M.spawn $
      "if type xmonad;"
       <> " then pkill xmobar; xmonad --recompile && xmonad --restart;"
       <> " else xmessage xmonad not in \\$PATH: \"$PATH\";"
       <> " fi"
    quitXMonad = do
      M.spawn "pkill xmobar"
      M.io (Exit.exitWith Exit.ExitSuccess)
    withSecurity key n action = confirm n
      -- execute the action only if the user hits `n` times `key`
      where
        confirm 0 = action
        confirm n
          | n > 0 = Submap.submap $ Map.fromList
              [(0, key) +++ confirm (n - 1)]
          | otherwise = action
    keypadWorkspaces = do
      (key, ws) <- zip numPadKeys ws
      [(mod, key) +++ M.windows (StackSet.view ws),
       (mod .|. shiftMask, key) +++ M.windows (StackSet.shift ws)]
    myHallway = Submap.submap (Rooms.roomsKeyMap_goto 0 myRooms)

--
-------------------------------------------------------
-- M O U S E M A P
-------------------------------------------------------

myMouse conf = Map.fromList
    $ (mod, button1)  +++ moveAfloat
    : (mod, button2)  +++ sinkIt -- set it as master
    : (mod .|. ctrlAlt, button2)  +++  killIt
    : (mod, button3)  +++ resizeAfloat
    : []
  where
    mod = M.modMask conf
    ctrlAlt = controlMask .|. mod1Mask
    moveAfloat w = M.focus w >> M.mouseMoveWindow w
      >> M.windows StackSet.shiftMaster
    sinkIt w = M.focus w >> M.windows (StackSet.sink w)
      >> M.windows StackSet.swapMaster
    killIt w = M.focus w >> M.kill
    resizeAfloat w = M.focus w >> M.mouseResizeWindow w
      >> M.windows StackSet.shiftMaster
