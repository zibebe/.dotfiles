local wezterm = require 'wezterm'
local config = wezterm.config_builder()

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Modus-Vivendi'
  else
    return 'Modus-Operandi'
  end
end

wezterm.on('window-config-reloaded', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  local appearance = window:get_appearance()
  local scheme = scheme_for_appearance(appearance)
  local colors = wezterm.color.get_builtin_schemes()[scheme]
  if overrides.color_scheme ~= scheme then
    overrides.color_scheme = scheme
    overrides.colors = {
      tab_bar = {
        background = colors.ansi[1],

        active_tab = {
          bg_color = colors.brights[1],
          fg_color = colors.foreground,
        },

        inactive_tab = {
          bg_color = colors.ansi[1],
          fg_color = colors.foreground,
        },

        inactive_tab_hover = {
          bg_color = colors.brights[1],
          fg_color = colors.foreground,
          italic = true,
        },

        new_tab = {
          bg_color = colors.ansi[1],
          fg_color = colors.foreground,
        },

        new_tab_hover = {
          bg_color = colors.brights[1],
          fg_color = colors.foreground,
          italic = true,
        },
      },
    }
    window:set_config_overrides(overrides)
  end
end)

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
