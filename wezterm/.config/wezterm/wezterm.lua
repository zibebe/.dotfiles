local wezterm = require("wezterm")
local mux = wezterm.mux
local config = wezterm.config_builder()

config.front_end = "WebGpu"
config.max_fps = 120
config.color_scheme = "Modus Vivendi"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font "Noto Sans Mono"
config.font_size = 18

wezterm.on('gui-startup', function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return config
