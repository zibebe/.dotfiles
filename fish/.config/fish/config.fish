if status is-interactive
    # Disable greeting
    set -g fish_greeting

    # Modern ls
    if command -q eza
        abbr -a ls eza
    end

    # Set fish highlighting according to system settings
    if test "$(defaults read -g AppleInterfaceStyle 2>/dev/null)" = Dark
        source ~/.config/fish/themes/modus_vivendi.fish
    else
        source ~/.config/fish/themes/modus_operandi.fish
    end

    # Load fnm
    fnm env --use-on-cd --shell fish | source

    # Load starship
    starship init fish | source
end
