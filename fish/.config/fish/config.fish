if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -g fish_greeting

if command -q eza
    abbr -a ls eza
end
