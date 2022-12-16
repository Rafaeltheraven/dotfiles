import XMonad
import qualified XMonad.Util.CustomKeys as C
import qualified Data.Map as M
import XMonad.StackSet as W
import XMonad.Layout.Tabbed
import XMonad.Layout
import XMonad.Layout.GridVariants
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows
import XMonad.Actions.WindowBringer
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.XPropManage
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Paste
import XMonad.Prompt
import System.Random
import System.Exit
import Data.List (sortBy, find)
import Data.Maybe (isNothing, maybeToList)
import Data.Function (on)
import Control.Monad (forM_, join, when)
import Data.Ratio

-- We start with some basic definitions
myTerminal = "termite"
myManageHook = (manageHook def <+> manageDocks) <+> manageScratchPad <+> specialHook <+> toggleHook' "toggle" toggleFriend namedHook
altMask = mod1Mask

-- |An XPConfig plucked from github, maybe improve later
myXPConfig :: XPConfig
myXPConfig =
  def { XMonad.Prompt.position = Top, font = "xft:DejaVu Sans:size=9", height = 40 }

-- |Basic scratchpad managehook stolen from somewhere, don't remember.
-- manageScratchPad :: ManageHook
-- manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
--    where
--        h = 0.3 -- terminal height 10%
--        w = 1 -- terminal width 100%
--        t = 0.020 -- distance from top edge
--        l = 1 - w -- distance from left edge

manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook scratchpads

-- | Float definitions for scratchpads
scratchFloat = customFloating $ W.RationalRect l t w h
    where
        h = 0.3 -- terminal height 30%
        w = 1 -- terminal width 100%
        t = 0.020 -- distance from top edge (for polybar)
        l = 1 - w -- distance from left edge

-- |Scratchpads, only really termite and alacritty flavors
scratchpads = 
    [ NS "termite" "termite --name scratchpad --role scratchpad" (role =? "scratchpad") scratchFloat
    , NS "alacritty" "alacritty --title scratchpad --class scratchpad" (title =? "scratchpad") scratchFloat
    ] 
    where
        role = stringProperty "WM_WINDOW_ROLE"


-- |Takes a tuple with a the name of a workspace and a set of window classNames,
-- mapping windows to specific workspaces. Returns a list of maybeManageHook to be used in composeOne
nameHelpFriend :: (String, [String]) -> [MaybeManageHook]
nameHelpFriend (name,names) = [className =? t -?> liftX (appendWorkspace name) >> doShift name | t <- names]

-- |A query to check if a certain window attribute is *not* found in the given list x.
querNotElem :: Eq a => Query a -> [a] -> Query Bool
querNotElem q x = fmap (not . (flip elem x)) q

-- |Special Cases
specialHook :: ManageHook
specialHook = composeAll
    [ className =? "firefoxdeveloperedition" <&&> resource =? "Toolkit" --> doRectFloat (RationalRect x y h w)
    , title =? "Picture-in-Picture" --> doF copyToAll
    ]
    where
        h = (1 % 4)
        w = (1 % 4)
        x = 1-w
        y = 1-h

-- |The big hook which does all the managing I want.
namedHook :: ManageHook
namedHook = composeOne $
    [transience] -- Fix floating "transient" windows
    ++ [stringProperty "WM_WINDOW_ROLE" =? "scratchpad" -?> idHook] -- Fuck you xmonad and fuck you x and fuck you termite as well
    ++ (concat (map nameHelpFriend specialBoys)) -- Map all the things in specialBoys
    ++ [querNotElem className floatyBoys -?> className >>= (\t -> liftX (appendWorkspace t) >> doShift t)] -- If a window is *not* part of the special float case, created a workspace with the window name and move the window there.
        where
            specialBoys =
                [("Internet", ["Firefox", "nightly", "Chromium", "Firefox Developer Edition", "firefoxdeveloperedition", "Epiphany", "Brave", "Brave-browser", "\"firefoxdeveloperedition\""])
                ,("Develop", ["Sublime Text 3", "subl", "Sublime Text", "Sublime_text", "jetbrains-idea", "Subl3"])
                ,("Chat", ["TelegramDesktop", "Whatsie", "nheko", "Signal"])
                ,("E-Mail", ["Thunderbird"])
                ,("Term", [myTerminal, "xterm", "Termite", "Alacritty"])
                ,("Reading", ["Evince"])
                ,("Keepass", ["keepassxc", "KeePassXC"])
                ,("Vidya", ["steam", "Steam", "Zenity"])
                ,("FFXIV", ["XIVLauncher.Core", "XIVLauncher", "ffxiv_dx11.exe", "ffxiv.exe"])
                ,("Office", ["libreoffice-writer", "libreoffice", "libreoffice-calc"])
                ,("VirtualBox", ["VirtualBox", "VirtualBox Manager", "VirtualBoxVM"])
                ]
            floatyBoys =
                ["nextcloud"
                ,"Polybar"
                ,"scratchpad"
                ,"polybar"
                ,"Gcr-prompter"
                ,"Xmessage"
--              ,"keepassxc"
--              ,"KeePassXC"
                -- ,"Steam"
                -- ,"steam"
                -- ,"Soffice"
                -- ,"Libreoffice"
                -- ,"Calc"
                -- ,"Writer"
                ,"Picture-in-Picture"
                ]

-- |A basic managehook used to toggle the big namedHook back on.
toggleFriend :: ManageHook
toggleFriend = idHook $ liftX (toggleHookNext "toggle")

main :: IO ()
main = do
    xmonad $ ewmhFullscreen $ ewmh $ addEwmhWorkspaceSort (pure $ filterOutWs ["NSP"]) $ docks def
       { borderWidth        = 0
       , terminal           = myTerminal
       , modMask = mod1Mask
       , keys = C.customKeys delkeys inskeys
       , startupHook = setWMName "LG3D" >> addHiddenWorkspace "NSP" >> addEWMHFullscreen >> spawn "~/.xmonad/autorun.sh"
       , manageHook = myManageHook
       , layoutHook = smartBorders . avoidStruts $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ layoutHook def
       , XMonad.workspaces = ["Term"]
       , handleEventHook = handleEventHook def
       }
        where
            mylayout = (Tall 1 (3/100) (1/2) ) ||| Grid (16/9) ||| simpleTabbed

            delkeys :: XConfig l -> [(KeyMask, KeySym)]
            delkeys XConfig {modMask = modm} =
                [ (modm , xK_q)
                , (modm .|. shiftMask, xK_q)
                , (modm , xK_j)
                , (modm , xK_k)
                ]

            inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
            inskeys conf@(XConfig {modMask = modm}) =
                let
                    -- envoirenment variables
                    font = "Monospace"
                    color = "-fg white -bg black"
                    -- applications
                    browser = "firefox-developer-edition"
                    terminal = myTerminal
                    filebrowser =  terminal ++ " -e ranger"
                    applauncher = "rofi -show drun"
                    sshlauncher = "rofi -show ssh"
                    calclauncher = "rofi -show calc"
                    textedit = "nano"
                    volumeMixer = terminal ++ " -e pulsemixer"
                    keeplauncher = "keepmenu"
                    mpdClient = "cantata"
                    mpdMenu = "mpdmenu -g -A -al -c :: -i"
                    mpdControl = "mpdmenu -C -toggle -stop -clear -prev -next -prevA -nextA -c :: -i"
                    tagEditor = "qoobar"
                    shotTool = "flameshot gui"
                    -- workspace opperations
                    swapHook :: X ()
                    swapHook = toggleHookNext "toggle"

                    swapHook2 :: X ()
                    swapHook2 = appendWorkspacePrompt myXPConfig >> toggleHookNext "toggle"

                    -- bindings

                    -- application bindings
                    appBinds =
                        [ ((modm, xK_b), spawn filebrowser)
                        , ((modm, xK_w), spawn browser)
                        , ((modm, xK_Return), spawn terminal)
                        , ((modm, xK_p), spawn applauncher)
                        , ((modm, xK_o), spawn calclauncher)
                        , ((modm, xK_s), spawn textedit)
--                        , ((modm, xK_z), scratchpadSpawnActionCustom ("alacritty --title scratchpad --class scratchpad")) -- Alacritty
                        , ((modm, xK_z), namedScratchpadAction scratchpads "termite") -- Termite
                        , ((modm .|. shiftMask, xK_p), spawn mpdMenu)
                        , ((modm .|. shiftMask, xK_c), spawn mpdControl)
                        , ((modm .|. shiftMask, xK_s), spawn sshlauncher)
                        , ((modm .|. shiftMask, xK_z), spawn "mpc toggle")
                        , ((modm .|. shiftMask, xK_k), spawn keeplauncher)
                        , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
                        , ((modm .|. shiftMask, xK_t), spawn "/home/rafael/.config/polybar/launch.sh")
                        ]

                    -- music
                    musicBinds =
                        [
                        ((modm , xK_m), submap . M.fromList $
                        [ ((0, xK_l),     spawn "mpa next")
                        , ((0, xK_k),     spawn "mpa prev")
                        , ((0, xK_p),     spawn mpdMenu)
                        , ((0, xK_z),     spawn "radio start")
                        , ((0, xK_space), spawn "mpc toggle")
                        , ((0, xK_c),     spawn "mpc clear")
                        , ((0, xK_u),     spawn "mpc rescan")
                        , ((0, xK_l),     spawn mpdClient)
                        , ((0, xK_t),     spawn tagEditor)
                        , ((0, xK_i),     spawn "mpc add https://ivory.thebias.nl; mpc play")
                        , ((0, xK_r),     spawn "mpc add https://radio.thebias.nl; mpc play")
                        ])
                        ]

                    ctrlToThisWs :: ((KeyMask, KeySym), X ()) -> ((KeyMask, KeySym), X ())
                    ctrlToThisWs ((mask,sym), thing) = ((mask .|. controlMask,sym), swapHook >> thing)

                    altToNewWS :: ((KeyMask, KeySym), X ()) -> ((KeyMask, KeySym), X ())
                    altToNewWS ((mask, sym), thing) = ((mask .|. altMask, sym), swapHook2 >> thing)
                    in
                    appBinds ++ musicBinds ++ (fmap ctrlToThisWs (appBinds ++ musicBinds)) ++ (fmap altToNewWS (appBinds ++ musicBinds)) ++ [ -- actual start of the bindings block
                    --
                    -- Miscelanious bindings (that do not require additional editing
                    --
                        -- focus, h & l
                        ((modm , xK_l), windows W.focusDown)
                        , ((modm , xK_h), windows W.focusUp)
                        , ((modm .|. shiftMask, xK_l), windows W.swapDown)
                        , ((modm .|. shiftMask, xK_h), windows W.swapUp)
                        , ((modm .|. controlMask, xK_l), sendMessage Expand)
                        , ((modm .|. controlMask, xK_h), sendMessage Shrink)
                        , ((modm, xK_f), sendMessage $ Toggle FULL)
                        , ((modm .|. shiftMask, xK_f), withFocused setWMHint)

                        -- workspaces
                        , ((modm , xK_slash), removeEmptyWorkspaceAfter $ gotoMenu)
                        , ((modm, xK_Left), moveTo Prev (WSIs notSP))
                        , ((modm .|. shiftMask, xK_Left), removeEmptyWorkspaceAfter $ shiftTo Prev (WSIs notSP))
                        , ((modm, xK_Right), moveTo Next (WSIs notSP))
                        , ((modm .|. shiftMask, xK_Right), removeEmptyWorkspaceAfter $ shiftTo Next (WSIs notSP))
                        , ((modm, xK_k), appendWorkspacePrompt myXPConfig)

                        -- me being a jackass
                        , ((modm , xK_q), kill >> (windows . shift) "NSP" >> removeEmptyWorkspaceAfter ((gets (currentTag . windowset)) >>= (\s -> whenX (isEmpty s) $ moveTo Prev (WSIs notSP))))
                        , ((modm .|. shiftMask, xK_q), kill1)

                        -- XF86 keys
                        , ((0, 0x1008FF11), spawn "pactl set-sink-volume 0 -5%")
                        , ((0, 0x1008FF13), spawn "pactl set-sink-volume 0 +5%")
                        , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle")
                        , ((0, 0x1008FFb2), spawn "pactl set-source-mute 1 toggle")
                        , ((modm, 0x1008FF12), spawn volumeMixer)

                        , ((0, 0x1008FF02), spawn "light -A 5")
                        , ((0, 0x1008FF03), spawn "light -U 5")
                        , ((shiftMask, 0x1008FF02), spawn "light -S 100")
                        , ((shiftMask, 0x1008FF03), spawn "light -S 0.1")

                        , ((0, xK_Print), spawn shotTool)
                    ]
                    where
                            notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
                            isEmpty :: String -> X Bool
                            isEmpty t = do wsl <- gets $ W.workspaces . windowset
                                           let mws = find (\ws -> tag ws == t) wsl
                                           return $ maybe True (isNothing . stack) mws


-- |Fuck Firefox
addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

-- |Adds EWMH _NET_WM_STATE_FULLSCREEN to given window, but no way to remove or actually update the layout yet.
setWMHint :: Window -> X ()
setWMHint w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_STATE"
    b <- getAtom "_NET_WM_STATE_FULLSCREEN"
    c <- getAtom "ATOM"
    io $ changeProperty32 dpy w a c propModeAppend [fromIntegral b]
