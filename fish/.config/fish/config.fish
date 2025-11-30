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

# eza config path
set -gx EZA_CONFIG_DIR $HOME/.config/eza

if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # fnm (node version manager)
    if type -q fnm
        fnm env --use-on-cd --shell fish | source
    end

    # modern ls
    if type -q eza
        abbr -a l eza
        abbr -a ls eza
        abbr -a ll 'eza -l'
        abbr -a lll 'eza -la'
    else
        abbr -a l ls
        abbr -a ll 'ls -l'
        abbr -a lll 'ls -la'
    end

    # modern cat
    if type -q bat
        abbr -a cat bat
    end

    # zoxide
    if type -q zoxide
        zoxide init fish | source
    end

    # note taking
    alias notes "hx $HOME/Notes/index.md"

    # starship
    if type -q starship
        starship init fish | source
    end
end
