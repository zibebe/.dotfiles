local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux

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
      bg_color = "#7e9cd8",
      fg_color = "#16161d",
      intensity = "Bold"
    },

    inactive_tab = {
      bg_color = "#16161d",
      fg_color = "#c8c093",
    },

    inactive_tab_hover = {
      bg_color = "#16161d",
      fg_color = "#7e9cd8",
    },

    new_tab = {
      bg_color = "#1f1f28",
      fg_color = "#c8c093",
    },

    new_tab_hover = {
      bg_color = "#1f1f28",
      fg_color = "#7e9cd8",
    },
  },
}

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
