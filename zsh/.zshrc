# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# enable completions
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

# keybindings
bindkey '^r' history-incremental-search-backward

# add kubectl completions
if type kubectl &> /dev/null; then
  source <(kubectl completion zsh)
fi

# add rustup completions
if type rustup &> /dev/null; then
  source <(rustup completions zsh)
fi

# fnm (node version manager)
if type fnm &> /dev/null; then
  eval "$(fnm env --use-on-cd --shell zsh)"
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
source $HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOMEBREW_PREFIX/share/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
