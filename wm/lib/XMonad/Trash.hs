{-# LANGUAGE FlexibleContexts #-}


module XMonad.Trash (
  trashWS)
  where

import qualified Data.Map as Map

import XMonad
import XMonad.MiscTools
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Actions.Submap as Submap

trashWS :: String -> X ()
trashWS trashName = withFocused $ \win -> do
    windows (StackSet.shift trashName)
    Submap.submap (confirming win)
  where
    confirming win = Map.fromList
      $ (0, xK_Escape) +++ (cancel win)
      : (0, xK_Return) +++ (delete win)
      : []
    cancel win = do
      ws <- gets currentWorkspace
      focus win
      windows $ StackSet.shift (StackSet.tag ws) -- put it back
      focus win
    delete win = do
      killWindow win
    --   b <- isWinAlive win
    --   if b then windows
    --       (StackSet.greedyView trashName) -- goto trash
    --     else pure ()
    -- isWinAlive win = do
    --   windowstack <- gets windowset
    --   pure (StackSet.member win windowstack)