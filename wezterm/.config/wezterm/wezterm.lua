local wezterm = require("wezterm")
local config = wezterm.config_builder()


local function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return "Dark"
end

local function scheme_for_appearance(appearance)
  local is_dark = appearance:find("Dark")

  local home = os.getenv("HOME")
  local helix_theme = is_dark and "modus_vivendi" or "modus_operandi"
  local fish_theme = is_dark and "modus_vivendi" or "modus_operandi"
  local wezterm_theme = is_dark and "Modus Vivendi" or "Modus Operandi"

  local fish_script_path = home .. string.format("/.config/fish/themes/%s.fish", fish_theme)
  local helix_config = home .. "/.config/helix/config.toml"

  -- Set fish theme
  os.execute(string.format("/opt/homebrew/bin/fish -c 'source %s'", fish_script_path))

  -- Check if helix theme needs updating
  local grep_cmd = string.format('grep -q \'theme = "%s"\' %s', helix_theme, helix_config)
  local theme_already_set = os.execute(grep_cmd) == 0

  -- Update helix theme if needed
  if not theme_already_set then
    local sed_cmd = string.format(
      'sed -i \'\' \'s|theme = "modus_.*"|theme = "%s"|\' %s',
      helix_theme,
      helix_config
    )
    os.execute(sed_cmd)
    os.execute("pkill -USR1 hx")
  end

  return wezterm_theme
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
