if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Load fnm
    fnm env --use-on-cd --shell fish | source

    # Load starship
    starship init fish | source
end
