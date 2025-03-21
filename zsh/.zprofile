# general globals
export LANG=en_US.UTF-8
export EDITOR=vim
export NAME='Tobias Tschinkowitz'
export EMAIL=me@zibebe.net
export TZ=Europe/Berlin

# homebrew
eval $(/opt/homebrew/bin/brew shellenv)

# go
export GOPATH="$HOME/Developer/go"
export PATH="$GOPATH/bin:$PATH"

# psql
export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"

# rust
. $HOME/.cargo/env
