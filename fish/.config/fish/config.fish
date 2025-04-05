if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Load fnm
    fnm env --use-on-cd --shell fish | source

    # Set up fzf key bindings
    fzf --fish | source

    # Nord Theme
    fish_config theme choose Nord

    # abbrevations
    if command -q eza then
        abbr -a ls eza
    end

    # Load starship
    starship init fish | source
end
