{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Rooms (
  Room(..),
  roomNames,
  roomsKeyMap_open,
  roomsKeyMap_goto,
  initRoomElse,
  simpleRoom,
  simpleRoom',
  simpleRooms,
  simpleRooms')
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
  roomKey :: Maybe KeySym,
  roomAction :: X ()
  }


openRoom :: Room -> X ()
openRoom room = do
  let name = roomName room
  windows (StackSet.greedyView name) -- try change ws/room
  ws <- gets currentWorkspace
  let roomIsOpen = (StackSet.tag ws == name)
  let roomIsEmpty = isNothing (StackSet.stack ws)
  let ok = roomIsOpen && roomIsEmpty
  when ok (roomAction room)

gotoRoom :: Room -> X ()
gotoRoom room = do
  let name = roomName room
  windows (StackSet.greedyView name) -- try change ws/room

initRoomElse :: [Room] -> X() -> X()
initRoomElse allRooms elseAction = do
  ws <- gets currentWorkspace
  let roomIsEmpty = isNothing (StackSet.stack ws)
  if roomIsEmpty
    then do
      let wsName = StackSet.tag ws
      case filter (\r -> roomName r == wsName) allRooms of
        [] -> pure () -- unrecognized workspace, do noth
        (r:_) -> roomAction r
    else elseAction

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

roomsKeyMap_open
  :: ButtonMask
  -> [Room]
  -> Map.Map (ButtonMask, KeySym) (X ())
roomsKeyMap_open modmask rooms = Map.fromList (f rooms)
  where
    f [] = []
    f (room:rooms) = let rec = f rooms in
      case roomKey room of
        Nothing -> rec
        Just key ->
          ((modmask, key), openRoom room) :
          ((modmask .|. shiftMask, key), withFocused $ sendToRoom room) : rec

roomsKeyMap_goto
  :: ButtonMask
  -> [Room]
  -> Map.Map (ButtonMask, KeySym) (X ())
roomsKeyMap_goto modmask rooms = Map.fromList (f rooms)
  where
    f [] = []
    f (room:rooms) = let rec = f rooms in
      case roomKey room of
        Nothing -> rec
        Just key ->
          ((modmask, key), gotoRoom room) :
          ((modmask .|. shiftMask, key), withFocused $ sendToRoom room) : rec

-------------------------- Tools
orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a

justList :: [Maybe a] -> [a]
justList (Just a:as) = a : justList as
justList (Nothing:as) = justList as
