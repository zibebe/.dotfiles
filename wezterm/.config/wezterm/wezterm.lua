local wezterm = require("wezterm")
local mux = wezterm.mux
local config = wezterm.config_builder()
local appearance = wezterm.gui.get_appearance()

local function scheme_for_appearance()
	if appearance:find "Dark" then
		os.execute([[sed -i '' 's/^theme = ".*"$/theme = "modus_vivendi"/' /Users/zibebe/.config/helix/config.toml]])
		os.execute("pkill -USR1 hx")
		os.execute([[/opt/homebrew/bin/fish -c 'echo y | fish_config theme save "Modus Vivendi"']])
		os.execute("cp /Users/zibebe/.config/eza/modus_vivendi.yml /Users/zibebe/.config/eza/theme.yml")
		os.execute("cp /Users/zibebe/.config/yazi/modus_vivendi.toml /Users/zibebe/.config/yazi/theme.toml")
		os.execute([[sed -i '' 's/^theme: .*$/theme: textual-dark/' /Users/zibebe/.config/posting/config.yaml]])
		return "Modus Vivendi"
	else
		os.execute([[sed -i '' 's/^theme = ".*"$/theme = "modus_operandi"/' /Users/zibebe/.config/helix/config.toml]])
		os.execute("pkill -USR1 hx")
		os.execute([[/opt/homebrew/bin/fish -c 'echo y | fish_config theme save "Modus Operandi"']])
		os.execute("cp /Users/zibebe/.config/eza/modus_operandi.yml /Users/zibebe/.config/eza/theme.yml")
		os.execute("cp /Users/zibebe/.config/yazi/modus_operandi.toml /Users/zibebe/.config/yazi/theme.toml")
		os.execute([[sed -i '' 's/^theme: .*$/theme: textual-light/' /Users/zibebe/.config/posting/config.yaml]])
		return "Modus Operandi"
	end
end

config.front_end = "WebGpu"
config.max_fps = 120
config.color_scheme = scheme_for_appearance()
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.window_decorations = "RESIZE"
config.audible_bell = "Disabled"
config.font = wezterm.font "SF Mono"
config.line_height = 1.15
config.font_size = 18.0

wezterm.on('gui-startup', function(cmd)
	local _, _, window = mux.spawn_window(cmd or {})
	window:gui_window():maximize()
end)

return config
