# editor
emacs_terminal() {
    emacs -nw "$@"
}
export EDITOR="emacs_terminal"
export VISUAL="emacs_terminal"

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

# haskell
. $HOME/.ghcup/env
