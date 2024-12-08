# general globals
export LANG=en_US.UTF-8
export EDITOR=hx
export NAME='Tobias Tschinkowitz'
export EMAIL=me@zibebe.net
export TZ=Europe/Berlin

# local bin
export PATH="$HOME/.local/bin:$PATH"

# go
export GOPATH="$HOME/Developer/go"
export PATH="$HOME/Developer/go/bin:$PATH"
export GOPRIVATE=github.com/scite-solutions/go-historian/historian
export GOTOOLCHAIN=local

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

# psql
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"
fi

# fzf settings
export FZF_DEFAULT_COMMAND='fd --type f --follow --exclude .git --exclude .DS_Store'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --follow --exclude .git'

# haskell
export PATH="$HOME/.ghcup/bin:$PATH"
