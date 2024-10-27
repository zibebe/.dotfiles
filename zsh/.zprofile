# general globals
export LANG=en_US.UTF-8
emacs_nw() { emacs -nw -- "$@" }
export EDITOR=emacs_nw
export VISUAL=emacs
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
