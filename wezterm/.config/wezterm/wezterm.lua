local wezterm = require 'wezterm'
local mux = wezterm.mux
local config = wezterm.config_builder()
local kanagawa_colors = wezterm.color.get_builtin_schemes()['Kanagawa (Gogh)']

config.color_scheme = 'Kanagawa (Gogh)'
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font 'JetBrains Mono'
config.font_size = 16.0
config.line_height = 1.1
config.front_end = 'WebGpu'
config.native_macos_fullscreen_mode = true
config.colors = {
  tab_bar = {
    background = kanagawa_colors.ansi[1],

    active_tab = {
      bg_color = kanagawa_colors.ansi[1],
      fg_color = kanagawa_colors.brights[8],
    },

    inactive_tab = {
      bg_color = kanagawa_colors.ansi[1],
      fg_color = kanagawa_colors.brights[1],
    },

    inactive_tab_hover = {
      bg_color = kanagawa_colors.ansi[1],
      fg_color = kanagawa_colors.brights[8],
      italic = true,
    },

    new_tab = {
      bg_color = kanagawa_colors.ansi[1],
      fg_color = kanagawa_colors.brights[1],
    },

    new_tab_hover = {
      bg_color = kanagawa_colors.ansi[1],
      fg_color = kanagawa_colors.brights[8],
      italic = true,
    },
  },
}

return config
