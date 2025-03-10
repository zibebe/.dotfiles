# general globals
export LANG=en_US.UTF-8
export EDITOR=vim
export NAME='Tobias Tschinkowitz'
export EMAIL=me@zibebe.net
export TZ=Europe/Berlin

# local bin
export PATH="$HOME/.local/bin:$PATH"

# go
export GOPATH="$HOME/Developer/go"
export PATH="$GOPATH/bin:$PATH"

# homebrew
eval $(/opt/homebrew/bin/brew shellenv)

# psql
export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"

# fixes ssl problems with python
export SSL_CERT_FILE="/etc/ssl/cert.pem"

# rust
. $HOME/.cargo/env
