# XMonad

My XMonad setup that tries to sort of replicate the functionality
found when using awesomewm with the tyrannical plugin. By this I mean
that there is a small group of set workspaces to which certain programs belong
and all the rest get their own workspace, all of which are dynamically created and
removed.

This setup makes extensive use of EWMH so if that doesn't work for you this won't either.

## Some Shortcuts
* `Super-Q` - Kill focused window, if this is the only window on the workspace, 
move to other workspace and delete old empty one.
* `Super-left/right` - Move to previous/next workspace.
* `Super-Ctrl-*` - Override managehook and spawn the next window on current workspace
regardless of what window it is.
* `Super-Alt-*` - Prompt for a new workspace name, create it and spawn the window on this workspace instead.
* `Super-Z` - Spawn dropdown terminal window.
