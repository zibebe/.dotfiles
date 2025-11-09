local wezterm = require("wezterm")
local mux = wezterm.mux
local config = wezterm.config_builder()

local scheme = wezterm.get_builtin_color_schemes()["nord"]

scheme.tab_bar = {
  background = scheme.background,

  active_tab = {
    bg_color = scheme.ansi[7],
    fg_color = scheme.ansi[1],
  },

  inactive_tab = {
    bg_color = scheme.ansi[1],
    fg_color = scheme.foreground,
  },

  inactive_tab_hover = {
    bg_color = scheme.ansi[1],
    fg_color = scheme.ansi[7],
  },

  new_tab = {
    bg_color = scheme.background,
    fg_color = scheme.foreground,
  },

  new_tab_hover = {
    bg_color = scheme.background,
    fg_color = scheme.ansi[7],
  },
}

config.color_schemes = {
  ["nord"] = scheme
}

config.front_end = "WebGpu"
config.max_fps = 120
config.color_scheme = "nord"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font "Comic Code Ligatures"
config.font_size = 18
config.line_height = 1.2

wezterm.on('gui-startup', function(cmd)
	local _, _, window = mux.spawn_window(cmd or {})
	window:gui_window():maximize()
end)

return config
