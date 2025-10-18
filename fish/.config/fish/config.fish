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

# k9s config path
set -gx K9S_CONFIG_DIR $HOME/.config/k9s

if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Nord Theme
    fish_config theme choose Nord

    # fnm (node version manager)
    if type -q fnm
        fnm env --use-on-cd --shell fish | source
    end

    # starship
    if type -q starship
        starship init fish | source
    end
end
