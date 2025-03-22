if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Modern ls
    if command -q eza
        abbr -a ls eza
    end

    # TokyoNight Night theme
    fish_config theme choose tokyonight_night

    # Load fnm
    fnm env --use-on-cd --shell fish | source

    # Load starship
    starship init fish | source
end


