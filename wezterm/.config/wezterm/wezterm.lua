local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.color_scheme = 'nord'
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font 'Fira Code Retina'
config.font_size = 16.0
config.line_height = 1.1
config.front_end = 'WebGpu'
config.native_macos_fullscreen_mode = true
config.window_decorations = 'RESIZE'
config.audible_bell = 'Disabled'

return config
