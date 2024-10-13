local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local colors = wezterm.color.get_builtin_schemes()['nord']

config.color_scheme = 'nord'
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font 'Fira Code Retina'
config.font_size = 16.0
config.line_height = 1.1
config.front_end = 'WebGpu'
config.native_macos_fullscreen_mode = true

config.colors = {
  tab_bar = {
    background = colors.background,
    active_tab = {
      bg_color = colors.ansi[5],
      fg_color = colors.ansi[1],
    },
    inactive_tab = {
      bg_color = colors.background,
      fg_color = colors.foreground,
    },
    inactive_tab_hover = {
      bg_color = colors.brights[5],
      fg_color = colors.brights[1],
      italic = true,
    },
    new_tab = {
      bg_color = colors.background,
      fg_color = colors.foreground,
    },
    new_tab_hover = {
      bg_color = colors.brights[5],
      fg_color = colors.brights[1],
      italic = true,
    },
  },
}

config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
}

return config
