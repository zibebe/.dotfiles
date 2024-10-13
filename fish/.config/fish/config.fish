# evaluate which os we are running
set OSTYPE (uname)

# General
set -x EDITOR hx
set -x LANG en_US.UTF-8
set -x NAME 'Tobias Tschinkowitz'
set -x EMAIL tobias.tschinkowitz@icloud.com
set -x TZ Europe/Berlin

# Homebrew
if test "$OSTYPE" = Darwin
    eval (/opt/homebrew/bin/brew shellenv)
end

# Use Brew LLVM
if test "$OSTYPE" = Darwin
    fish_add_path /opt/homebrew/llvm/bin
    set -x LDFLAGS -L/opt/homebrew/opt/llvm/lib
    set -x CPPFLAGS -I/opt/homebrew/opt/llvm/include
end

# Rust
set -x RUST_BACKTRACE 1

# go
set -x GOPATH "$HOME/Developer/go"
fish_add_path /home/tobias/Developer/go/bin

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
    set -x MOZ_ENABLE_WAYLAND 1
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
set -x FZF_DEFAULT_COMMAND 'fd --type f --follow'
set -x FZF_CTRL_T_COMMAND 'fd --type f --follow'
set -x FZF_ALT_C_COMMAND 'fd --type d --follow'
set -x FZF_DEFAULT_OPTS '--height 20%'
# Scheme name: Nord
# Scheme system: base16
# Scheme author: arcticicestudio
# Template author: Tinted Theming (https://github.com/tinted-theming)
set -x FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS"\
" --color=bg+:#3b4252,bg:#2e3440,spinner:#88c0d0,hl:#81a1c1"\
" --color=fg:#d8dee9,header:#81a1c1,info:#ebcb8b,pointer:#88c0d0"\
" --color=marker:#88c0d0,fg+:#eceff4,prompt:#ebcb8b,hl+:#81a1c1"

# Fish git prompt
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showdirtystate yes

if status is-interactive
    # Commands to run in interactive sessions can go here
    set -g fish_greeting
end

if status is-login
    if test "$OSTYPE" = Linux -a -z "$WAYLAND_DISPLAY" -a "$XDG_VTNR" = 1
        exec dbus-run-session ssh-agent sway
    end
end
