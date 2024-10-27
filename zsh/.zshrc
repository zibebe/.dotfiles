# add homebrew app completions to fpath
if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

# enable completions and prompt
autoload -Uz compinit
compinit

# history
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE
setopt SHARE_HISTORY

# aliases
alias emacs='emacs -nw'

# add kubectl completions
if type kubectl &> /dev/null; then
	source <(kubectl completion zsh)
fi

# fnm (node version manager)
if type fnm &> /dev/null; then
	eval "$(fnm env --use-on-cd)"
fi

# zsh syntax highlighting
if [[ "$OSTYPE" == "darwin"* ]]; then
	source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
