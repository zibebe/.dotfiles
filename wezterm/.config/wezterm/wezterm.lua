local wezterm = require("wezterm")
local config = wezterm.config_builder()

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'modus-vivendi'
  else
    return 'modus-operandi'
  end
end

wezterm.on('window-config-reloaded', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  local appearance = window:get_appearance()
  local scheme = scheme_for_appearance(appearance)
  if overrides.color_scheme ~= scheme then
    overrides.color_scheme = scheme
    window:set_config_overrides(overrides)
  end
end)

config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font("SF Mono")
config.font_size = 16.0
config.line_height = 1.1
config.front_end = "WebGpu"
config.native_macos_fullscreen_mode = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"

return config
