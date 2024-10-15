local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local colors = wezterm.color.get_builtin_schemes()['Kanagawa (Gogh)']

config.color_scheme = 'Kanagawa (Gogh)'
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font 'Fira Code'
config.font_size = 16.0
config.line_height = 1.1
config.front_end = 'WebGpu'
config.native_macos_fullscreen_mode = true
config.window_decorations = 'RESIZE'

config.colors = {
  tab_bar = {
    background = colors.ansi[1],

    active_tab = {
      bg_color = colors.ansi[1],
      fg_color = colors.brights[8],
    },

    inactive_tab = {
      bg_color = colors.ansi[1],
      fg_color = colors.brights[1],
    },

    inactive_tab_hover = {
      bg_color = colors.brights[1],
      fg_color = colors.ansi[8],
      italic = true,
    },

    new_tab = {
      bg_color = colors.ansi[1],
      fg_color = colors.brights[1],
    },

    new_tab_hover = {
      bg_color = colors.brights[1],
      fg_color = colors.ansi[8],
      italic = true,
    },
  },
}

config.keys = {
  {
    key = 'w',
    mods = 'SHIFT|CTRL',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
}

return config
