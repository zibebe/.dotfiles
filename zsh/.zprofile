# general globals
export EDITOR=hx

# local binaries
export PATH="$HOME/.local/bin:$PATH"

# homebrew
eval $(/opt/homebrew/bin/brew shellenv)

# go
export GOPATH="$HOME/Developer/go"
export PATH="$GOPATH/bin:$PATH"

# psql
export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"

# rust
. $HOME/.cargo/env

# helix runtime path
export HELIX_RUNTIME="$HOME/Developer/rust/helix/runtime"
