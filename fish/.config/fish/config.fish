# Various globals
set -gx EDITOR hx

# Local binaries
fish_add_path $HOME/.local/bin

# Homebrew
/opt/homebrew/bin/brew shellenv | source

# Go
set -gx GOPATH $HOME/Developer/go
fish_add_path $GOPATH/bin

# PostgreSQL (libpq)
fish_add_path $HOMEBREW_PREFIX/opt/libpq/bin

# Rust
fish_add_path $HOME/.cargo/bin

# Haskell
fish_add_path $HOME/.ghcup/bin
fish_add_path $HOME/.cabal/bin

# Helix runtime path
set -gx HELIX_RUNTIME $HOME/Developer/rust/helix/runtime

if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Nord Theme
    fish_config theme choose Nord

    # Note taking with helix
    alias notes="hx -w $HOME/Documents/Notes $HOME/Documents/Notes"

    # Modern ls
    if type -q eza
        abbr -a ls eza
    end

    # fnm (node version manager)
    fnm env --use-on-cd --shell fish | source

    # k9s config path
    set -gx K9S_CONFIG_DIR $HOME/.config/k9s

    # Load starship
    starship init fish | source
    enable_transience
end
