#!/bin/zsh

# source POSIX ENV
[ -n "$ENV" ] && . "$ENV"

# zsh settings
set -o inc_append_history
set -o hist_ignore_all_dups
set -o hist_ignore_space
SAVEHIST=2000
HISTSIZE=2000
HISTFILE="$XDG_CACHE_HOME/zsh_history"
zle_highlight+=(paste:none)

# by default you can't open URLs with '?' in them... 🤔
set -o no_nomatch

# enable better compeletion & reverse-menu-complete
autoload -Uz compinit && compinit
bindkey '^[[Z' reverse-menu-complete

# enable dircolors
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# set cool prompt
__git_prompt() {
    BRANCH="$(git branch 2>/dev/null | awk '/^\*/{$1=""; $0=$0; $1=$1; print}')"
    if [ -n "$BRANCH" ]; then
        if [ -n "$(git status --short 2>/dev/null)" ]; then
            printf " [%%F{red}%%B%s%%f]" "$BRANCH"
        else
            printf " [%%F{green}%s%%f]" "$BRANCH"
        fi
    fi
}

# I don't know how zsh's PROMPT works, so use PS1
set -o prompt_subst
PS1='%n@%m:%F{cyan}%~%f$(__git_prompt)> '

# nice man colors
export LESS_TERMCAP_mb=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_md=$'\e[1m\e[38;5;202m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1m\e[38;5;201m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1m\e[38;5;193m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1
