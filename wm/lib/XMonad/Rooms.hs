{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Rooms (
  Room(..),
  withRooms,
  roomNames,
  roomsKeyMap,
  -- startRoom,
  startRoomHere,
  simpleRoom,
  simpleRoom',
  simpleRooms,
  simpleRooms',
  openRoom)
  where

import Data.Maybe (isNothing)
import Control.Monad (when)
import qualified Data.Map as Map

import XMonad
import qualified XMonad.StackSet as StackSet

import XMonad.Layout.PerWorkspace as PerWS

-------- my own modules
import XMonad.MiscTools (currentWorkspace)

data Room = Room {
  roomName :: String,
  -- roomDir  :: String,
  -- roomLayout :: Maybe (Layout Window),
  roomKey :: Maybe KeySym,
  roomAction :: X ()
  }
  -- deriving (Eq, Show)

withRooms :: [Room] -> XConfig l -> XConfig l
withRooms rooms cfg = cfg {
  workspaces = roomNames rooms,
  -- layoutHook = withRoomLayouts rooms (layoutHook cfg),
  keys = \c -> Map.union (roomsKeyMap (modMask cfg) rooms) (keys cfg c)
  }

-- todel:
-- startRoom :: Room -> X ()
-- startRoom room = roomAction room

startRoomHere :: [Room] -> X ()
startRoomHere rooms = do
  name <- gets (StackSet.tag . currentWorkspace)
  case filter (\room -> name == roomName room) rooms of
    [] -> pure ()
    (room:_) -> roomAction room

openRoom :: Room -> X ()
openRoom room = do
  let name = roomName room
  windows (StackSet.greedyView name) -- try change ws/room
  ws <- gets currentWorkspace
  let roomIsOpen = (StackSet.tag ws == name)
  let roomIsEmpty = isNothing (StackSet.stack ws)
  let ok = roomIsOpen && roomIsEmpty
  when ok (roomAction room)

sendToRoom :: Room -> Window -> X ()
sendToRoom room focusedWindow = do
  windows $ StackSet.shift (roomName room)
  openRoom room

roomNames :: [Room] -> [String]
roomNames = fmap roomName


simpleRoom :: Show a => a -> KeySym -> Room
simpleRoom name key = simpleRoom' (show name) key

simpleRoom' :: String -> KeySym -> Room
simpleRoom' name key = Room name (Just key) (pure ())

simpleRooms :: Show a => [a] -> [KeySym] -> [Room]
simpleRooms names keys = fmap
    (uncurry simpleRoom)
    (zip names keys)

simpleRooms' :: [String] -> [KeySym] -> [Room]
simpleRooms' names keys = fmap
    (uncurry simpleRoom')
    (zip names keys)

roomsKeyMap :: ButtonMask -> [Room] -> Map.Map (ButtonMask, KeySym) (X ())
roomsKeyMap modmask rooms = Map.fromList (f rooms)
  where
    f [] = []
    f (room:rooms) = let rec = f rooms in
      case roomKey room of
        Nothing -> rec
        Just key ->
          ((modmask, key), openRoom room) :
          ((modmask .|. shiftMask, key), withFocused $ sendToRoom room) : rec


-------------------------- Tools
orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a

justList :: [Maybe a] -> [a]
justList (Just a:as) = a : justList as
justList (Nothing:as) = justList as