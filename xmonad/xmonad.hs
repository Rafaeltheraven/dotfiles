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
import XMonad.Util.Scratchpad
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

myTerminal = "termite"
myManageHook = (manageHook defaultConfig <+> manageDocks) <+> manageScratchPad <+> toggleHook' "toggle" nestedToggle namedHook
altMask = mod1Mask

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.3 -- terminal height 10%
        w = 1 -- terminal width 100%
        t = 0.025 -- distance from top edge
        l = 1 - w -- distance from left edge

nameHelpFriend :: (String, [String]) -> [MaybeManageHook]
nameHelpFriend (name,names) = [className =? t -?> liftX (appendWorkspace name) >> doShift name | t <- names]

querNotElem :: Eq a => Query a -> [a] -> Query Bool
querNotElem q x = fmap (not . (flip elem x)) q

namedHook :: ManageHook
namedHook = composeOne $
    [transience]
    ++ [stringProperty "WM_WINDOW_ROLE" =? "scratchpad" -?> idHook] -- Fuck you xmonad and fuck you x and fuck you termite as well
    ++ (concat (map nameHelpFriend specialBoys))
    ++ [querNotElem className floatyBoys -?> className >>= (\t -> liftX (appendWorkspace t) >> doShift t)]
        where
            specialBoys =
                [("Internet", ["Firefox", "nightly", "Chromium", "Firefox Developer Edition", "firefoxdeveloperedition", "Epiphany"])
                ,("Office", ["Soffice", "libreoffice-writer"])
                ,("Develop", ["Sublime Text 3", "subl", "Sublime Text", "Sublime_text", "jetbrains-idea"])
                ,("Chat", ["TelegramDesktop", "Whatsie", "nheko"])
                ,("E-Mail", ["Thunderbird"])
                ,("Term", [myTerminal, "xterm", "Termite"])
                ,("Reading", ["Evince"])
                ,("Keepass", ["keepassxc", "KeePassXC"])
                ]
            floatyBoys =
            	["nextcloud"
            	,"Polybar"
            	,"scratchpad"
            	,"polybar"
            	,"Gcr-prompter"
            	,"Xmessage"
--            	,"keepassxc"
--            	,"KeePassXC"
            	]

nestedToggle :: ManageHook
nestedToggle = toggleHook' "newWS" newWSFriend toggleFriend
--nestedToggle = toggleHook' "newWS" toggleFriend newWSFriend

toggleFriend :: ManageHook
toggleFriend = idHook $ liftX (toggleHookNext "toggle")

newWSFriend :: ManageHook
newWSFriend = idHook $ liftX (appendWorkspacePrompt defaultXPConfig) >> liftX (toggleHookNext "newWS") >> liftX (toggleHookNext "toggle")

main :: IO ()
main = do
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
        safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad $ ewmh $ docks def
       { borderWidth        = 0
       , terminal           = myTerminal
       , modMask = mod4Mask
       , keys = C.customKeys delkeys inskeys
       , startupHook = setWMName "LG3D" >> addHiddenWorkspace "NSP" >> addEWMHFullscreen >> spawn "~/.xmonad/autorun.sh"
    --      , manageHook = manageDocks <+> manageHook def
    --      , layoutHook = avoidStruts $ mylayout
       , manageHook = myManageHook
       , layoutHook = smartBorders . avoidStruts $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ layoutHook defaultConfig
       , XMonad.workspaces = ["Term"]
    --       , normalBorderColor = "#000000"
    --       , focusedBorderColor = "#444444"
       --, logHook = fadeInactiveLogHook 0.95
       , logHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
       , handleEventHook = handleEventHook def <+> ewmhDesktopsEventHook  <+> fullscreenEventHook
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
                    textedit = "nano"
                    volumeMixer = terminal ++ " -e pulsemixer"
                    keeplauncher = "keepmenu"
                    mpdClient = "cantata"
                    mpdMenu = "mpdmenu -g -A -al -c :: -i"
                    mpdControl = "mpdmenu -C -toggle -stop -clear -prev -next -prevA -nextA -c :: -i"
                    tagEditor = "qoobar"
                    -- workspace opperations
                    swapHook :: X ()
                    swapHook = toggleHookNext "toggle"

                    swapHook2 :: X ()
                    swapHook2 = toggleHookNext "toggle" >> toggleHookNext "newWS"

                    -- bindings

                    -- application bindings
                    appBinds =
                        [ ((modm, xK_b), spawn filebrowser)
                        , ((modm, xK_w), spawn browser)
                        , ((modm, xK_Return), spawn terminal)
                        , ((modm, xK_p), spawn applauncher)
                        , ((modm, xK_s), spawn textedit)
                        , ((modm, xK_z), scratchpadSpawnActionCustom (myTerminal ++ " --name scratchpad --role scratchpad"))
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

                        -- me being a jackass
                        , ((modm , xK_q), kill >> (windows . shift) "NSP" >> removeEmptyWorkspaceAfter ((gets (currentTag . windowset)) >>= (\s -> whenX (isEmpty s) $ moveTo Prev (WSIs notSP))))
                        , ((modm .|. shiftMask, xK_q), kill1)

                        -- XF86 keys
                        , ((0, 0x1008FF11), spawn "pactl set-sink-volume 0 -5%")
                        , ((0, 0x1008FF13), spawn "pactl set-sink-volume 0 +5%")
                        , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle")
                        , ((0, 0x1008FFb2), spawn "pactl set-source-mute 1 toggle")
                        , ((modm, 0x1008FF12), spawn volumeMixer)

                        , ((0, 0x1008FF02), spawn "xbacklight -inc 5")
                        , ((0, 0x1008FF03), spawn "xbacklight -dec 5")
                        , ((shiftMask, 0x1008FF02), spawn "xbacklight -set 100")
                        , ((shiftMask, 0x1008FF03), spawn "xbacklight -set 0.1")
                    ]
                    where
                            notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
                            isEmpty :: String -> X Bool
                            isEmpty t = do wsl <- gets $ W.workspaces . windowset
                                           let mws = find (\ws -> tag ws == t) wsl
                                           return $ maybe True (isNothing . stack) mws



-- Polybar Workspace shit

-- eventLogHook = do
--   winset <- gets windowset
--   title <- maybe (return "") (fmap show . getName) . W.peek $ winset
--   let currWs = W.currentTag winset
--   let wss = map W.tag $ scratchpadFilterOutWorkspace $ W.workspaces winset
--   let wsStr = join $ map (fmt currWs) $ sort' wss

--   io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
--   io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

--   where fmt currWs ws
--           | currWs == ws = "[" ++ ws ++ "]"
--           | otherwise    = " " ++ ws ++ " "
--         sort' = sortBy (compare `on` (!! 0))
-- --        sort' x = rotate (-(length x `quot` 2)) x
-- --        sort' x = x
--         rotate :: Int -> [a] -> [a]
--         rotate _ [] = []
--         rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Fuck Firefox
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

-- Adds EWMH _NET_WM_STATE_FULLSCREEN to given window, but no way to remove or actually update the layout yet.
setWMHint :: Window -> X ()
setWMHint w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_STATE"
    b <- getAtom "_NET_WM_STATE_FULLSCREEN"
    c <- getAtom "ATOM"
    io $ changeProperty32 dpy w a c propModeAppend [fromIntegral b]
