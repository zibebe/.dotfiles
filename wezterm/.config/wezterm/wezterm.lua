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
config.font = wezterm.font("Codelia Ligatures")
config.line_height = 1.1
config.font_size = 18.0
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"

wezterm.on('toggle-font', function(window, _)
  local overrides = window:get_config_overrides() or {}

  local current_font_family

  if overrides.font then
    current_font_family = overrides.font.font[1].family
  else
    current_font_family = config.font.font[1].family
  end

  if current_font_family == "Codelia Ligatures" then
    overrides.font = wezterm.font("Comic Code Ligatures")
    overrides.line_height = 1.2
  else
    overrides.font = wezterm.font("Codelia Ligatures")
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
