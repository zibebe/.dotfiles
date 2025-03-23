# Modus Themes for Fish
# Auto generated with https://github.com/miikanissi/modus-themes.nvim/blob/master/lua/modus-themes/extras/fish.lua

set -l foreground ffffff
set -l selection 7030af
set -l comment 989898
set -l red ff5f59
set -l orange fec43f
set -l yellow d0bc00
set -l green 44bc44
set -l purple b6a0ff
set -l cyan 00d3d0
set -l pink feacd0

# Syntax Highlighting Colors
set -U fish_color_normal $foreground
set -U fish_color_command $cyan
set -U fish_color_keyword $pink
set -U fish_color_quote $yellow
set -U fish_color_redirection $foreground
set -U fish_color_end $orange
set -U fish_color_option $pink
set -U fish_color_error $red
set -U fish_color_param $purple
set -U fish_color_comment $comment
set -U fish_color_selection --background=$selection
set -U fish_color_search_match --background=$selection
set -U fish_color_operator $green
set -U fish_color_escape $pink
set -U fish_color_autosuggestion $comment

# Completion Pager Colors
set -U fish_pager_color_progress $comment
set -U fish_pager_color_prefix $cyan
set -U fish_pager_color_completion $foreground
set -U fish_pager_color_description $comment
set -U fish_pager_color_selected_background --background=$selection
