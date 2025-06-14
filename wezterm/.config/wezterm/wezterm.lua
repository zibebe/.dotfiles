local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux
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

config.color_scheme = "nord"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font 'Comic Code Ligatures'
config.font_size = 18.0
config.line_height = 1.2

wezterm.on('gui-startup', function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

wezterm.on('update-right-status', function(window, _)
  local date = wezterm.strftime '%a %b %-d %H:%M'

  window:set_right_status(wezterm.format {
    { Background = { Color = scheme.ansi[7] } },
    { Foreground = { Color = scheme.ansi[1] } },
    { Text = ' ' .. date .. ' ' },
  })
end)

return config
