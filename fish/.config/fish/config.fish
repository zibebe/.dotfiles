if status is-interactive
    # Commands to run in interactive sessions can go here

    # Disable greeting
    set -g fish_greeting
end

# evaluate which os we are running
set OSTYPE (uname)

# General
set -gx EDITOR hx
set -gx LANG en_US.UTF-8
set -gx NAME 'Tobias Tschinkowitz'
set -gx EMAIL tobias.tschinkowitz@icloud.com
set -gx TZ Europe/Berlin

# Homebrew
if test "$OSTYPE" = Darwin
    eval (/opt/homebrew/bin/brew shellenv)
end

# Rust
source $HOME/.cargo/env.fish
set -gx RUST_BACKTRACE 1

# go
set -gx GOPATH "$HOME/Developer/go"
fish_add_path "$HOME/Developer/go/bin"

# python binaries
if test "$OSTYPE" = Darwin
    fish_add_path "$HOME/Library/Python/3.9/bin"
end

# postgresql client
if test "$OSTYPE" = Darwin
    fish_add_path "$HOMEBREW_PREFIX/opt/libpq/bin"
end

# Enable Wayland for Mozilla stuff
if test "$OSTYPE" = Linux
    set -gx MOZ_ENABLE_WAYLAND 1
end

# Add Flatpaks
if test "$OSTYPE" = Linux
    fish_add_path /var/lib/flatpak/exports/bin
end

# fnm
if command -v fnm >/dev/null
    fnm env | source
end

# Better ls
if command -v eza >/dev/null
    abbr -a l eza
    abbr -a ls eza
    abbr -a ll 'eza -l'
    abbr -a lll 'eza -la'
else
    abbr -a l ls
    abbr -a ll 'ls -l'
    abbr -a lll 'ls -la'
end

# various abbreviations
abbr -a lg lazygit

# fzf settings
fzf --fish | source
set -gx FZF_DEFAULT_COMMAND 'fd --type f --follow'
set -gx FZF_CTRL_T_COMMAND 'fd --type f --follow'
set -gx FZF_ALT_C_COMMAND 'fd --type d --follow'
set -gx FZF_DEFAULT_OPTS '--height 20%'
# Scheme name: Nord
# Scheme system: base16
# Scheme author: arcticicestudio
# Template author: Tinted Theming (https://github.com/tinted-theming)
set -gx FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS"\
" --color=bg+:#3b4252,bg:#2e3440,spinner:#88c0d0,hl:#81a1c1"\
" --color=fg:#d8dee9,header:#81a1c1,info:#ebcb8b,pointer:#88c0d0"\
" --color=marker:#88c0d0,fg+:#eceff4,prompt:#ebcb8b,hl+:#81a1c1"
