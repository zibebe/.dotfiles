# add homebrew app completions to fpath
if type brew &>/dev/null
then
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

# enable completions and prompt
autoload -Uz compinit
compinit

# add kubectl completions
if type kubectl &> /dev/null; then
  source <(kubectl completion zsh)
fi

# fnm (node version manager)
if type fnm &> /dev/null; then
  eval "$(fnm env --use-on-cd)"
fi
