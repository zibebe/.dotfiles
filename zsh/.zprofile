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
export PATH="$HOME/Developer/go/bin:$PATH"
export GOPRIVATE=github.com/scite-solutions/go-historian/historian
export GOTOOLCHAIN=local

# macos specific
if [[ "$OSTYPE" == "darwin"* ]]; then
  # homebrew
  eval $(/opt/homebrew/bin/brew shellenv)
  # llvm
  export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
  # psql
  export PATH="$HOMEBREW_PREFIX/opt/libpq/bin:$PATH"
  # fixes ssl problems with python
  export SSL_CERT_FILE="/etc/ssl/cert.pem" 
  # homebrew installed vim
  export PATH="$HOMEBREW_PREFIX/opt/vim/bin:$PATH"
fi

# rust
. $HOME/.cargo/env
export RUST_BACKTRACE=1

# fzf settings
export FZF_DEFAULT_COMMAND='fd --type f --follow --exclude .git --exclude .DS_Store'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --follow --exclude .git'
