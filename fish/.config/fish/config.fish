# Various globals
setenv EDITOR hx

# Local binaries
fish_add_path $HOME/.local/bin

# Homebrew
/opt/homebrew/bin/brew shellenv | source

# Go
setenv GOPATH $HOME/Developer/go
fish_add_path $GOPATH/bin

# PostgreSQL (libpq)
fish_add_path $HOMEBREW_PREFIX/opt/libpq/bin

# Rust
fish_add_path $HOME/.cargo/bin

# EZA
setenv EZA_CONFIG_DIR $HOME/.config/eza

if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Fish git prompt
    set __fish_git_prompt_showuntrackedfiles yes
    set __fish_git_prompt_showdirtystate yes
    set __fish_git_prompt_showstashstate yes
    set __fish_git_prompt_showupstream informative

    # Various useful abbreviations
    abbr -a c cargo
    abbr -a ct 'cargo test'
    abbr -a e hx
    abbr -a g git
    abbr -a gc 'git checkout'
    abbr -a ga 'git add -p'
    abbr -a gah 'git stash; and git pull --rebase; and git stash pop'
    abbr -a pr 'gh pr create -t "$(git show -s --format=%s HEAD)" -b "$(git show -s --format=%B HEAD | tail -n+3)"'

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

    # Allow Ctrl-z to toggle between suspend and resume
    bind \cz 'fg 2>/dev/null; commandline -f repaint'

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
