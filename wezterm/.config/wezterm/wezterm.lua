local wezterm = require("wezterm")
local config = wezterm.config_builder()
local scheme = wezterm.get_builtin_color_schemes()["nord"]

scheme.tab_bar = {
  background = scheme.background,

  active_tab = {
    bg_color = scheme.ansi[5],
    fg_color = scheme.ansi[1],
  },

  inactive_tab = {
    bg_color = scheme.ansi[1],
    fg_color = scheme.foreground,
  },

  inactive_tab_hover = {
    bg_color = scheme.ansi[1],
    fg_color = scheme.ansi[5],
  },

  new_tab = {
    bg_color = scheme.background,
    fg_color = scheme.foreground,
  },

  new_tab_hover = {
    bg_color = scheme.background,
    fg_color = scheme.ansi[5],
  },
}

config.color_schemes = {
  ["nord"] = scheme
}

config.color_scheme = "nord"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font("Fira Code Retina")
config.font_size = 16.0
config.line_height = 1.1
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"

return config
