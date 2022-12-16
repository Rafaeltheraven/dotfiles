#!/bin/bash -v

function run {
    if ! pgrep $1
    then
    	if [ "$1" != "firefox-developer-edition" ]
	then
		$@&
	elif ! pgrep "firefox"
	then
		$@&
	fi
    fi
}

run ~/.config/polybar/launch.sh
#run alacritty
run termite
#run telegram-desktop
run nm-applet
run redshift-gtk
#run xcompmgr
run keepassxc
#cadence-session-start --system-start
run optimus-manager-qt
run mailnag
run firefox-developer-edition
#exec setxkbmap -layout us -variant euro
#picom -b
setxkbmap us -variant altgr-intl
