local wezterm = require 'wezterm';
local act = wezterm.action

return {
  -- m1 path
  -- default_prog = { '/opt/homebrew/bin/nu' },
  default_prog = { '/usr/local/bin/nu' },

  font = wezterm.font 'Fira Code',

  -- color_scheme = "Gruvbox Light",
  color_scheme = "Japanesque",
  -- color_scheme = "Tomorrow Night Burns",
  -- color_scheme = "Zenburn",
  -- color_scheme = "Subliminal",
  -- color_scheme = "Seafoam Pastel",
  -- color_scheme = "Paraiso Dark",
  -- color_scheme = "Operator Mono Dark",
  -- color_scheme = "Gruvbox Dark",
  -- color_scheme = "Flatland",
  -- color_scheme = "Ciapre",
  -- color_scheme = "BirdsOfParadise",
  -- color_scheme = "Belafonte Night",
  -- color_scheme = "Belafonte Day",
  -- color_scheme = "Novel",

  leader = {key = "Space", mods = "CTRL"},
  keys = {
    {
      mods = "LEADER",
      key = "-",
      action=act{SplitVertical={domain="CurrentPaneDomain"}}
    },
    {
      mods = "LEADER",
      key = "/",
      action=act{SplitHorizontal={domain="CurrentPaneDomain"}}
    },
    {
      key = 'r',
      mods = 'LEADER',
      action = act{ActivateKeyTable={
        name = 'resize_pane',
        one_shot = false,
        replace_current=false,
      }},
    },

    -- CTRL+SHIFT+Space, followed by 'w' will put us in activate-pane
    -- mode until we press some other key or until 1 second (1000ms)
    -- of time elapses
    {
      key = 'w',
      mods = 'LEADER',
      action = act{ActivateKeyTable={
        name = 'activate_pane',
        timeout_milliseconds = 1000,
        one_shot = false,
        replace_current=false,
      }},
    },
    {
      key = 'w',
      mods = 'CMD',
      action = act{CloseCurrentPane={ confirm = true }},
    },
  },
  key_tables = {
    -- Defines the keys that are active in our resize-pane mode.
    -- Since we're likely to want to make multiple adjustments,
    -- we made the activation one_shot=false. We therefore need
    -- to define a key assignment for getting out of this mode.
    -- 'resize_pane' here corresponds to the name="resize_pane" in
    -- the key assignments above.
    resize_pane = {
      { key = 'LeftArrow', action = act{AdjustPaneSize={ 'Left', 1 }} },
      { key = 'h', action = act{AdjustPaneSize={ 'Left', 1 }} },

      { key = 'RightArrow', action = act{AdjustPaneSize={ 'Right', 1 }} },
      { key = 'l', action = act{AdjustPaneSize={ 'Right', 1 }} },

      { key = 'UpArrow', action = act{AdjustPaneSize={ 'Up', 1 }} },
      { key = 'k', action = act{AdjustPaneSize={ 'Up', 1 }} },

      { key = 'DownArrow', action = act{AdjustPaneSize={ 'Down', 1 }} },
      { key = 'j', action = act{AdjustPaneSize={ 'Down', 1 }} },

      -- Cancel the mode by pressing escape
      { key = 'Escape', action = 'PopKeyTable' },
    },

    -- Defines the keys that are active in our activate-pane mode.
    -- 'activate_pane' here corresponds to the name="activate_pane" in
    -- the key assignments above.
    activate_pane = {
      { key = 'LeftArrow', action = act{ActivatePaneDirection='Left'} },
      { key = 'h', action = act{ActivatePaneDirection='Left'} },

      { key = 'RightArrow', action = act{ActivatePaneDirection='Right'} },
      { key = 'l', action = act{ActivatePaneDirection='Right'} },

      { key = 'UpArrow', action = act{ActivatePaneDirection='Up'} },
      { key = 'k', action = act{ActivatePaneDirection='Up'} },

      { key = 'DownArrow', action = act{ActivatePaneDirection='Down'} },
      { key = 'j', action = act{ActivatePaneDirection='Down'} },
    },
  },
}
