# completion setup
fpath+=$HOME/.zfunc
autoload -Uz compinit
compinit

# history file configuration
HISTFILE=$HOME/.zsh_history
HISTSIZE=50000
SAVEHIST=10000

# history command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data

# fnm setup
if type fnm &> /dev/null; then
  eval "$(fnm env --use-on-cd --shell zsh)"
fi

# aliases
alias emacs="emacs_terminal"
