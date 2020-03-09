#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

if [ -e ~/.bashrc.aliases ] ; then
   source ~/.bashrc.aliases
fi

if [ -e /usr/share/bash-completion/completions/mpc ] ; then
   source /usr/share/bash-completion/completions/mpc
fi

# >>> Added by cnchi installer
BROWSER=/usr/bin/firefox-developer-edition
EDITOR=/usr/bin/nano

# Fix path or something
export PATH=$PATH:/home/rafael/.local/bin:~/.cargo/bin:~/bins

# SSH-Agent pls
#~/Documents/fix_ssh.sh

eval "$(thefuck --alias)"
