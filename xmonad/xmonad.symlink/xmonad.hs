import XMonad

import Data.Monoid
-- import Control.Monad

import XMonad.Util.Run
import XMonad.Actions.CycleWS
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout.WindowNavigation

import XMonad.Hooks.ManageDocks -- avoidStruts
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns


-- MULTIMONITOR
import XMonad.Actions.PhysicalScreens

-- Workspace prompt:
--http://hackage.haskell.org/package/xmonad-contrib-0.7/docs/XMonad-Actions-DynamicWorkspaces.html
import XMonad.Prompt
import XMonad.Actions.DynamicWorkspaces

import XMonad.Actions.ShowText
-- xmonad { handleEventHook = myHandleEventHooks <+> handleTimerEvent }

-- Trying to get xmobar working
import XMonad.Util.Run(spawnPipe)


myTextConfig :: ShowTextConfig
myTextConfig = STC
	{ st_font = "-*-montecarlo-medium-r-normal-*-22-*-*-*-*-*-*-*"
	, st_bg   = "#020202"
	, st_fg   = "#a9a6af"
	}

-- Define the names of all workspaces
myWorkspaces =
    ["web-test","web-doc","web-me","emacs","db","cmd","unit","misc1","misc2",
     "misc3"]

handleEventHook :: Event -> X All
handleEventHook _ = return (All True)

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    Full |||
    ThreeColMid 1 (3/100) (1/2)
    -- multiCol [0] 1 0.01 0.5
    -- noBorders (fullscreenFull Full)
  )

myManageHook = composeAll
    [ className =? "Gimp"          --> doFloat ]
	<+> manageDocks

-- Run XMonad
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/nathan/.xmobarrc"
    xmonad $ defaultConfig {
        modMask = mod4Mask
        , terminal = "urxvt"
        , borderWidth = 2
        , focusFollowsMouse = False
        , keys = myKeys
        , workspaces = myWorkspaces
        , layoutHook = smartBorders myLayout
        , manageHook = myManageHook
        -- , handleEventHook =  <+> handleTimerEvent
        -- , handleEventHook = defaultConfig <+> handleTimerEvent

    }


-- Define keys to add
keysToAdd x =
    [
         -- ((mod4Mask, xK_Insert), nextWS >> logCurrent >>= flashText myTextConfig 1 . fromMaybe "")
         ((mod4Mask, xK_Insert), flashText myTextConfig 1 "Moved to next Workspace" >> nextWS),

        -- Emacs-like key-bindings
           ((modMask x, xK_p), windows W.focusUp)
         , ((modMask x, xK_n), windows W.focusDown)
         , (((modMask x .|. controlMask), xK_p), windows W.swapUp)
         , (((modMask x .|. controlMask), xK_n), windows W.swapDown)

         -- Switching focus to different screens (for triple monitors)
         -- Stolen from:
         -- https://github.com/Libbum/xmonad/blob/master/xmonad.hs
         , ((mod4Mask                     , xK_w), viewScreen 0)
         , ((mod4Mask                     , xK_v), viewScreen 1)
         , ((mod4Mask                     , xK_z), viewScreen 2)
         , ((mod4Mask .|. shiftMask       , xK_w), sendToScreen 0)
         , ((mod4Mask .|. shiftMask       , xK_v), sendToScreen 1)
         , ((mod4Mask .|. shiftMask       , xK_z), sendToScreen 2)

        -- Workspace prompt
         , ((mod4Mask , xK_m), selectWorkspace defaultXPConfig)
         , ((mod4Mask .|. shiftMask, xK_m), withWorkspace defaultXPConfig (windows . W.shift))
         , ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace)

        -- Dmenu with different keys
       , ((modMask x, xK_apostrophe), spawn "dmenu_run")

         -- Close window
        ,  ((modMask x, xK_c), kill)

        -- -- Shift window to next workspace (leaving this here to remember how to use shift for keybindings)
        -- ,  (((modMask x .|. shiftMask), xK_Right), shiftToNext)
    ]

-- Define keys to remove
keysToRemove x =
    [
           (modMask x, xK_m)
         , (modMask x, xK_n)
        -- Remove old dmenu binding
         , (modMask x, xK_p)
        -- Unused gmrun binding
         , (modMask x .|. shiftMask, xK_p)
        -- Unused close window binding
        , (modMask x .|. shiftMask, xK_c)
        -- Old way of switching between dual screens
         , (modMask x, xK_w)
         , (modMask x, xK_e)
    ]

-- Delete the keys combinations we want to remove.
strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- Compose all my new key combinations.
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))


-- Emacs config
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/sykopomp%27s_xmonad.hs

-- Make emacs and xmonad aware of each other.
-- https://github.com/emacsattic/xmonad/blob/master/xmonad-emacs.cabal
