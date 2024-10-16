# general globals
export LANG=en_US.UTF-8
export EDITOR=hx
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

# llvm on macos
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
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
# Scheme name: Kanagawa
# Scheme system: base16
# Scheme author: Tommaso Laurenzi (https://github.com/rebelot)
# Template author: Tinted Theming (https://github.com/tinted-theming)

# export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
# " --color=bg+:#16161d,bg:#1f1f28,spinner:#6a9589,hl:#7e9cd8"\
# " --color=fg:#727169,header:#7e9cd8,info:#c0a36e,pointer:#6a9589"\
# " --color=marker:#6a9589,fg+:#c8c093,prompt:#c0a36e,hl+:#7e9cd8"

# Scheme name: Nord
# Scheme system: base16
# Scheme author: arcticicestudio
# Template author: Tinted Theming (https://github.com/tinted-theming)

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"\
" --color=bg+:#3b4252,bg:#2e3440,spinner:#88c0d0,hl:#81a1c1"\
" --color=fg:#d8dee9,header:#81a1c1,info:#ebcb8b,pointer:#88c0d0"\
" --color=marker:#88c0d0,fg+:#eceff4,prompt:#ebcb8b,hl+:#81a1c1"
