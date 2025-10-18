local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux

config.color_scheme = "Dracula (Official)"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
-- config.font = wezterm.font 'Comic Code Ligatures'
-- config.line_height = 1.2
config.font = wezterm.font 'Codelia Ligatures'
config.line_height = 1.1
config.font_size = 18.0
config.send_composed_key_when_left_alt_is_pressed = true
config.send_composed_key_when_right_alt_is_pressed = false

wezterm.on('gui-startup', function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return config
