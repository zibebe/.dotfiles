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

# aliases
alias emacs=emacs_terminal

# add kubectl completions
if type kubectl &> /dev/null; then
  source <(kubectl completion zsh)
fi

# fnm (node version manager)
if type fnm &> /dev/null; then
  eval "$(fnm env --use-on-cd)"
fi

# fzf
if type fzf &> /dev/null; then
  source <(fzf --zsh)
fi

# ollama sync models helper
ollama_sync() {
    ollama list | tail -n +2 | awk '{print $1}' | while read -r model; do
        ollama pull $model
    done
}

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

# using ripgrep combined with preview
# find-in-file - usage: fif <searchTerm>
fif() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  rg --files-with-matches --no-messages "$1" | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

# alias to merge multiple .mp4 files in the current directory
alias merge_mp4='ffmpeg -f concat -safe 0 -i <(for f in ./*.mp4; do echo "file '\''$PWD/$f'\''"; done | sort -V) -c copy merged.mp4'

# zsh syntax highlighting
if [[ "$OSTYPE" == "darwin"* ]]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# powerlevel10k
source ~/Developer/zsh/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
