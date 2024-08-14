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

# aliases
alias lg=lazygit

# helper functions
combine_mp4() {
	for FILE in *.mp4
	do echo file "'$FILE'" >> list.txt
	done
  ffmpeg -f concat -i list.txt -c copy output.mp4
}

# better ls
if type eza &> /dev/null; then
	alias l=eza
	alias ls=eza
	alias ll='eza -l'
	alias lll='eza -la'
else
	alias l=ls
	alias ll='ls -l'
	alias lll='ls -la'
fi

# add kubectl completions 
source <(kubectl completion zsh)

# fnm (node version manager)
eval "$(fnm env --use-on-cd)"

# zsh syntax highlighting
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zsh autosuggestions
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# powerlevel10k theme
source /opt/homebrew/share/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
