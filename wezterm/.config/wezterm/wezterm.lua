local wezterm = require("wezterm")
local config = wezterm.config_builder()

config.front_end = "WebGpu"
config.max_fps = 120
config.color_scheme = "Gruvbox dark, hard (base16)"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font "SF Mono"
config.line_height = 1.3
config.font_size = 18

config.keys = {
  {
    key = "Enter",
    mods = "SHIFT",
    action = wezterm.action.SendString("\x1b[13;2u"),
  }
}

return config
