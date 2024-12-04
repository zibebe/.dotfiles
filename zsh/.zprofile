# general globals
export LANG=en_US.UTF-8
export EDITOR=emacs
export VISUAL=emacs
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

# gnu awk and grep on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="/opt/homebrew/opt/gawk/libexec/gnubin:$PATH"
  export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
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
export FZF_DEFAULT_OPTS='--height 20%'

# haskell
export PATH="$HOME/.ghcup/bin:$PATH"
