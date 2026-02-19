local wezterm = require("wezterm")
local config = wezterm.config_builder()
local scheme = wezterm.get_builtin_color_schemes()["Gruvbox dark, hard (base16)"]

scheme.tab_bar = {
  background = scheme.indexed[19],

  active_tab = {
    bg_color = "#d79921",
    fg_color = scheme.background,
  },

  inactive_tab = {
    bg_color = scheme.indexed[18],
    fg_color = scheme.indexed[21],
  },

  inactive_tab_hover = {
    bg_color = scheme.indexed[19],
    fg_color = "#d79921",
  },

  new_tab = {
    bg_color = scheme.indexed[18],
    fg_color = scheme.indexed[21],
  },

  new_tab_hover = {
    bg_color = scheme.indexed[18],
    fg_color = scheme.ansi[4],
  },
}

config.color_schemes = {
  ["Gruvbox dark, hard (base16)"] = scheme
}

config.front_end = "WebGpu"
config.max_fps = 120
config.color_scheme = "Gruvbox dark, hard (base16)"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font "Noto Sans Mono"
config.font_size = 18

config.keys = {
  {
    key = "Enter",
    mods = "SHIFT",
    action = wezterm.action.SendString("\x1b[13;2u"),
  }
}

return config
