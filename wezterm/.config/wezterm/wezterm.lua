local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux
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
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font 'Comic Code Ligatures'
config.line_height = 1.2
config.font_size = 18.0

wezterm.on('gui-startup', function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

wezterm.on('toggle-font', function(window, _)
  local overrides = window:get_config_overrides() or {}

  local current_config = overrides.font or config.font
  local current_family = current_config and current_config.font and current_config.font[1] and
      current_config.font[1].family or ""

  if current_family == "Codelia Ligatures" then
    overrides.font = wezterm.font 'Comic Code Ligatures'
    overrides.line_height = 1.2
  else
    overrides.font = wezterm.font 'Codelia Ligatures'
    overrides.line_height = 1.1
  end

  window:set_config_overrides(overrides)
end)

config.keys = {
  {
    key = 'f',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.EmitEvent 'toggle-font',
  },
}

return config
