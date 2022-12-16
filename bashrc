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
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin"

eval "$(thefuck --alias)"

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

export SOFTHSM2_CONF=/home/rafael/Documents/BRBA/91divoc-ln/harrie-0/softhsm/softhsm2.conf
export VISUAL=nano

export PS1="[\[$(tput sgr0)\]\[\033[38;5;83m\]\u\[$(tput sgr0)\]\[\033[38;5;13m\]@\[$(tput sgr0)\]\[\033[38;5;196m\]\h\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;229m\]\$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /')\[\033[38;5;14m\]\W\[$(tput sgr0)\]]\\$ \[$(tput sgr0)\]"
