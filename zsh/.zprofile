# general globals
export LANG=en_US.UTF-8
export EDITOR=vi
export NAME='Tobias Tschinkowitz'
export EMAIL=tobias.tschinkowitz@icloud.com
export TZ=Europe/Berlin

# go
export GOPATH="$HOME/Developer/go"
export PATH="$HOME/Developer/go/bin:$PATH"

# homebrew
if [[ "$OSTYPE" == "darwin"* ]]; then
  eval $(/opt/homebrew/bin/brew shellenv)
fi

# doom-emacs
export PATH="$HOME/.config/emacs/bin:$PATH"

# gnu-grep
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
fi

# rust
. $HOME/.cargo/env
export RUST_BACKTRACE=1

# python binaries
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="$HOME/Library/Python/3.9/bin:$PATH"
fi

# psql
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"
fi

# Enable Wayland for Mozilla stuff
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export MOZ_ENABLE_WAYLAND=1
fi

# Add Flatpaks
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export PATH="/var/lib/flatpak/exports/bin:$PATH"
fi

# fzf settings
export FZF_DEFAULT_COMMAND='fd --type f --follow'
export FZF_CTRL_T_COMMAND='fd --type f --follow'
export FZF_ALT_C_COMMAND='fd --type d --follow'
export FZF_DEFAULT_OPTS='--height 20%'
# Scheme name: Nord
# Scheme system: base16
# Scheme author: arcticicestudio
# Template author: Tinted Theming (https://github.com/tinted-theming)

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
" --color=bg+:#3b4252,bg:#2e3440,spinner:#88c0d0,hl:#81a1c1"\
" --color=fg:#d8dee9,header:#81a1c1,info:#ebcb8b,pointer:#88c0d0"\
" --color=marker:#88c0d0,fg+:#eceff4,prompt:#ebcb8b,hl+:#81a1c1"
