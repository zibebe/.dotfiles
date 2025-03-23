local wezterm = require("wezterm")
local config = wezterm.config_builder()

local function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return "Dark"
end

local function scheme_for_appearance(appearance)
  if appearance:find "Dark" then
    return "Modus Vivendi"
  else
    return "Modus Operandi"
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.font = wezterm.font("Fira Code Retina")
config.font_size = 16.0
config.line_height = 1.1
config.front_end = "WebGpu"
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"

return config
