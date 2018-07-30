
module XMonad.MiscTools where

import XMonad
import qualified XMonad.StackSet as StackSet

currentWorkspace = StackSet.workspace
  . StackSet.current . windowset

numPadKeys = xK_KP_End   :xK_KP_Down  :xK_KP_Page_Down -- 1, 2, 3
             :xK_KP_Left :xK_KP_Begin :xK_KP_Right     -- 4, 5, 6
             :xK_KP_Home :xK_KP_Up    :xK_KP_Page_Up   -- 7, 8, 9
             :xK_KP_Insert :[]                            -- 0


a +++ b = (a, b)
infixr 9 +++

toEnglish :: Int -> String
toEnglish 0 = "ZERO"
toEnglish 1 = "ONE"
toEnglish 2 = "TWO"
toEnglish 3 = "THREE"
toEnglish 4 = "FOUR"
toEnglish 5 = "FIVE"
toEnglish 6 = "SIX"
toEnglish 7 = "SEVEN"
toEnglish 8 = "EIGHT"
toEnglish 9 = "NINE"
toEnglish 10 = "TEN"
toEnglish n = show n



awesomeFolder = "\xf07c"
awesomeGlobe = "\xf0ac"
awesomePlayCircle = "\xf01d"
awesomePlaySquare = "\xf16a"
awesomeMusic = "\xf001"
awesomePlay = "\xf04b"
awesomeCode = "\xf121"
awesomeImage = "\xf03e"
awesomeTerminal = "\xf120"
awesomeCheckSquare = "\xf046"

awesomeMicroChip = "\xf2db" -- 4.7
awesomeWifi = "\xf1eb"
awesomeVolumeFull = "\xf028"
awesomeSettings = "\xf1de"
awesomeTree = "\xf0e8"
awesomeArrowIn = "\xf090"
awesomeArrowOut = "\xf08b"
awesomeShield = "\xf132"
awesomeMenu = "\xf0c9"
awesomePlug = "\xf1e6"
awesomeCloudWifi = "\xf289"
awesomeLink = "\xf0c1"
awesomeBrokenLink = "\xf127" -- ChainBroken
awesomeKeyboard = "\xf11c"
awesomeLaptop = "\xf109"
awesomeDesktop = "\xf108"
awesomeInbox = "\xf01c"
awesomeHeartbeat = "\xf21e"
awesomeHDD = "\xf0a0"
awesomeGears = "\xf085"
awesomeFirefox = "\xf269"
awesomeFilm = "\xf008"
awesomeEnvelope = "\xf0e0"
awesomeCodeFork = "\xf126"
awesomeCoffee = "\xf0f4"
awesomeCogs = "\xf013"
awesomeDiskPile = "\xf1c0" --database
awesomeCloud = "\xf0c2"
awesomeClock = "\xf017"
awesomeCube = "\xf1b2"
awesomeCubes = "\xf1b3"
awesomeDashboard = "\xf0e4"
awesomeChrome = "\xf268"
awesomeCalendar = "\xf073"
awesomeBolt = "\xf0e7"
awesomeSun = "\xf185"
awesomeAreaChart = "\xf1fe"
awesomeBarChart = "\xf080"
awesomeBarChart2 = "\xf012" -- signal