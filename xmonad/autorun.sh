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

run firefox-developer-edition
run termite
#run telegram-desktop
run nm-applet
run redshift-gtk
run xcompmgr
run keepassxc
#cadence-session-start --system-start
~/.config/polybar/launch.sh
exec setxkbmap -layout us -variant euro
