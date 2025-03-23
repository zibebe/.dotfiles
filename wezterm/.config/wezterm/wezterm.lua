local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- Kanagwa Color Scheme took from
-- https://github.com/rebelot/kanagawa.nvim/blob/master/extras/wezterm/kanagawa.lua
config.force_reverse_video_cursor = true
config.colors = {
  foreground = "#dcd7ba",
  background = "#1f1f28",

  cursor_bg = "#c8c093",
  cursor_fg = "#c8c093",
  cursor_border = "#c8c093",

  selection_fg = "#c8c093",
  selection_bg = "#2d4f67",

  scrollbar_thumb = "#16161d",
  split = "#16161d",

  ansi = { "#090618", "#c34043", "#76946a", "#c0a36e", "#7e9cd8", "#957fb8", "#6a9589", "#c8c093" },
  brights = { "#727169", "#e82424", "#98bb6c", "#e6c384", "#7fb4ca", "#938aa9", "#7aa89f", "#dcd7ba" },
  indexed = { [16] = "#ffa066", [17] = "#ff5d62" },

  tab_bar = {
    background = "#1f1f28",
    active_tab = {
      fg_color = "#1f1f28",
      bg_color = "#c0a36e",
    },
    inactive_tab = {
      fg_color = "#1f1f28",
      bg_color = "#727169",
    },
    inactive_tab_hover = {
      fg_color = "#c0a36e",
      bg_color = "#727169",
    },
    new_tab = {
      fg_color = "#c0a36e",
      bg_color = "#1f1f28",
    },
    new_tab_hover = {
      fg_color = "#c0a36e",
      bg_color = "#1f1f28",
    }
  }
}
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font("Fira Code Retina")
config.font_size = 16.0
config.line_height = 1.1
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"

return config
