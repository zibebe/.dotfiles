# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

# add kubectl completions
if type kubectl &> /dev/null; then
  source <(kubectl completion zsh)
fi

# fnm (node version manager)
if type fnm &> /dev/null; then
  eval "$(fnm env --use-on-cd)"
fi

# eza
if type eza &> /dev/null; then
  alias l='eza'
  alias ls='eza'
  alias ll='eza -l'
  alias lll='eza -la'
else
  alias l='ls'
  alias ll='ls -l'
  alias lll='ls -la'
fi

# zsh plugins
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOMEBREW_PREFIX/share/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
