-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.My.Theme (
  logger,
  topBar,
  mainConfig)
  where

import Data.Char (toUpper)

import qualified XMonad as M
import qualified XMonad.Hooks.DynamicLog as DL

import qualified XMonad.Layout.Decoration as Deco

------------- Themes
logger = DL.xmobarPP
    { DL.ppCurrent
        = fgbg black silver
        . wrapped "  "
        . fmap toUpper
    , DL.ppTitle = fg dimWhite . bef "  "
    , DL.ppSep = ""
    , DL.ppHiddenNoWindows = hidden
    , DL.ppHidden = hidden
    , DL.ppLayout = hidden
    }
  where
    fg x = DL.xmobarColor x ""
    bg x = DL.xmobarColor "" x
    fgbg x y = DL.xmobarColor x y
    wrapped x = DL.wrap x x
    bef x = DL.wrap x ""
    aft x = DL.wrap "" x
    hidden = const ""

mainConfig config =
    config
      { M.normalBorderColor = black
      , M.focusedBorderColor = active
      , M.borderWidth = 1
      }

topBar = Deco.def
    { Deco.inactiveBorderColor   = darkRed
    , Deco.inactiveColor         = darkRed
    , Deco.inactiveTextColor     = darkRed
    , Deco.activeBorderColor     = active
    , Deco.activeColor           = active
    , Deco.activeTextColor       = active
    , Deco.urgentBorderColor     = orange
    , Deco.urgentTextColor       = orange
    , Deco.urgentColor           = orange
    , Deco.decoHeight            = 12
    }

------------- Private Values
active = pinkRed


------------- Colors
pinkRed = "#f92472"
black = "black"
orange = "orange"
silver = "#aaa"
dimWhite = "#ddd"
darkRed = "#500"

