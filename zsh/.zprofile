# general globals
export LANG=en_US.UTF-8
export EDITOR=vim
export NAME='Tobias Tschinkowitz'
export EMAIL=me@zibebe.net
export TZ=Europe/Berlin
export GPG_TTY=$(tty)

# local bin
export PATH="$HOME/.local/bin:$PATH"

# go
export GOPATH="$HOME/Developer/go"
export PATH="$HOME/Developer/go/bin:$PATH"
export GOPRIVATE=github.com/scite-solutions/go-historian/historian
export GOTOOLCHAIN=local

# macos specific
if [[ "$OSTYPE" == "darwin"* ]]; then
  # homebrew
  eval $(/opt/homebrew/bin/brew shellenv)
  # psql
  export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"
  # fixes ssl problems with python
  export SSL_CERT_FILE="/etc/ssl/cert.pem" 
fi

# rust
. $HOME/.cargo/env
export RUST_BACKTRACE=1
