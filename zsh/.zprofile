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
eval $(/opt/homebrew/bin/brew shellenv)

# rust
. "$HOME/.cargo/env"
export RUST_BACKTRACE=1

# python binaries
export PATH="$HOME/Library/Python/3.9/bin:$PATH"

# psql
export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"
