# Various globals
setenv EDITOR vim 

# Local binaries
fish_add_path $HOME/.local/bin

# Homebrew
/opt/homebrew/bin/brew shellenv | source
setenv HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS 1

# Go
setenv GOPATH $HOME/Developer/go
setenv GOPRIVATE "github.com/scite-solutions/*"
fish_add_path $GOPATH/bin

# PostgreSQL (libpq)
fish_add_path $(brew --prefix libpq)/bin

# Rust
source "$HOME/.cargo/env.fish"

# EZA
setenv EZA_CONFIG_DIR $HOME/.config/eza

if status is-interactive
    # Getting rid of "last login" message and fish's greeting
    set -g fish_greeting

    # Fish git prompt
    set __fish_git_prompt_showuntrackedfiles yes
    set __fish_git_prompt_showdirtystate yes
    set __fish_git_prompt_showstashstate yes
    set __fish_git_prompt_showupstream informative

    # eza (modern ls)
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

    # fzf (fuzzy file finder)
    if type -q fzf
        fzf --fish | source
        setenv FZF_DEFAULT_COMMAND 'fd --type file --follow'
        setenv FZF_CTRL_T_COMMAND 'fd --type file --follow'
    end

    # fnm (node version manager)
    if type -q fnm
        fnm env --use-on-cd --shell fish | source
    end

    # zoxide (smater cd)
    if type -q zoxide
        zoxide init fish | source
    end
end
