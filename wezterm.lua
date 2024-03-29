-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

config.window_background_opacity = 0.3
config.allow_square_glyphs_to_overflow_width = "Never"
config.window_close_confirmation = "NeverPrompt"
config.window_decorations = "NONE"
config.hide_tab_bar_if_only_one_tab = true
config.font = wezterm.font 'Hack Nerd Font'
config.font_size = 8
config.default_gui_startup_args = { 'start','/bin/bash' }
config.color_scheme = 'Banana Blueberry'


config.keys = {
  {
    key = 'n',
    mods = 'ALT',
    action = wezterm.action.SplitPane {
      direction = 'Right',
      size = { Percent = 50 },
    },
  },
  {
    key = 'd',
    mods = 'ALT',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
}

-- For example, changing the color scheme:


-- and finally, return the configuration to wezterm
return config
