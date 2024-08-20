# general globals
export LANG=en_US.UTF-8
export EDITOR=hx
export NAME='Tobias Tschinkowitz'
export EMAIL=tobias.tschinkowitz@icloud.com
export TZ=Europe/Berlin
export TERM=screen-256color

# go
export GOPATH="$HOME/Developer/go"
export PATH="$HOME/Developer/go/bin:$PATH"

# homebrew
if [[ "$OSTYPE" == "darwin"* ]]; then
  eval $(/opt/homebrew/bin/brew shellenv)
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
