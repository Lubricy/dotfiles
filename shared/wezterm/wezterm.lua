-- wezterm.lua -- Corrected Configuration

-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action


-- This table will hold the configuration.
local config = {}

config.front_end = "WebGpu"

-- In newer versions of WezTerm, use the config_builder
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Set the default terminal type, analogous to `set -g default-terminal`
-- config.term = 'screen-256color'
config.launch_menu = {}
-- Leader key configuration, similar to tmux `prefix`
-- Your tmux.conf sets escape-time to 10ms for the prefix
config.leader = { key = '`', mods = 'NONE', timeout_milliseconds = 1000 }

-- Mimics `set -s escape-time 0`. This is the general timeout for key sequences.
config.send_composed_key_when_left_alt_is_pressed = true

-- **FIXED**: Set the base index for tabs and panes to 1, like `set -g base-index 1`
-- We do this by telling WezTerm the numbering is NOT zero-based.
config.tab_and_split_indices_are_zero_based = false

-- Enable mouse control globally, equivalent to `set -g mouse on`
-- This single setting enables clicking panes, tabs, resizing, and scroll wheel support.
-- config.enable_mouse = true

-- HELPER FUNCTION: Convert a file URI from OSC 7 to a native Windows path
local function get_native_path_from_uri(uri)
  if not uri then return nil end
  -- This function handles URIs like "file:///C:/Users/Foo" on Windows
  if wezterm.target_triple:find("windows") then
    -- 1. Remove the "file:///" prefix, leaving "C:/Users/Foo"
    uri = tostring(uri)
    local path = uri:gsub("^file:///", "")
    -- 2. Convert all forward slashes to backslashes for Windows
    return path:gsub("/", "\\")
  else
    -- For Linux/macOS, just remove the "file://" prefix
    return uri:gsub("^file://", "")
  end
end

config.set_environment_variables = {
  WSLENV = 'WEZTERM_PANE',
}

-- Key bindings
config.keys = {
  -- Send the leader key itself by pressing it twice
  { key = '`', mods = 'LEADER',       action = act.SendKey{ key = '`' } },
  { key = "-", mods = "LEADER",       action=wezterm.action.SplitVertical {domain="CurrentPaneDomain"}},
  { key = "\\",mods = "LEADER",       action=wezterm.action.SplitHorizontal {domain="CurrentPaneDomain"}},
  { key = "c", mods = "LEADER",       action=wezterm.action.SpawnCommandInNewTab {
      cwd = "~"
    }
  },
  { key = "z", mods = "LEADER",       action="TogglePaneZoomState" },
  { key = "x", mods = "LEADER",       action=wezterm.action{CloseCurrentPane={confirm=true}}},
  { key = "1", mods = "LEADER",       action=wezterm.action{ActivateTab=0}},
  { key = "2", mods = "LEADER",       action=wezterm.action{ActivateTab=1}},
  { key = "3", mods = "LEADER",       action=wezterm.action{ActivateTab=2}},
  { key = "4", mods = "LEADER",       action=wezterm.action{ActivateTab=3}},
  { key = "5", mods = "LEADER",       action=wezterm.action{ActivateTab=4}},
  { key = "6", mods = "LEADER",       action=wezterm.action{ActivateTab=5}},
  { key = "7", mods = "LEADER",       action=wezterm.action{ActivateTab=6}},
  { key = "8", mods = "LEADER",       action=wezterm.action{ActivateTab=7}},
  { key = "9", mods = "LEADER",       action=wezterm.action{ActivateTab=8}},
  { key = 'LeftArrow',  mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Left"}},
  { key = 'DownArrow',  mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Down"}},
  { key = "UpArrow",    mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Up"}},
  { key = 'RightArrow', mods = "LEADER",       action=wezterm.action{ActivatePaneDirection="Right"}},
  -- A more interactive way to "join" a pane
  { key = 'j', mods = 'LEADER', action = act.PaneSelect { mode = "SwapWithActive" } },

  -- Reload configuration, like `bind R`
  { key = 'R', mods = 'LEADER', action = act.ReloadConfiguration },

  -- Swap pane with the next one, similar to `bind r` (swap-pane -U)
  { key = 'r', mods = 'LEADER', action = act.RotatePanes "Clockwise" },

  -- Toggle mouse control with `LEADER` + `m`, like `bind m set -g mouse`
  {
    key = 'm',
    mods = 'LEADER',
    action = wezterm.action_callback(function(window, pane)
      local overrides = window:get_config_overrides() or {}
      local current_mouse_state = overrides.enable_mouse
      -- If not overridden, get the state from the main config
      if current_mouse_state == nil then
        current_mouse_state = config.enable_mouse
      end

      local new_state = not current_mouse_state
      overrides.enable_mouse = new_state
      window:set_config_overrides(overrides)

      -- Show a status message toast
      local status_text = new_state and "Mouse Enabled" or "Mouse Disabled"
      wezterm.toast_notification("Mouse Toggle", status_text, nil, 2000)
    end),
  },

  -- Pane resizing without a leader key
  { key = 'UpArrow',    mods = 'CTRL',        action = act.AdjustPaneSize { 'Up', 1 } },
  { key = 'DownArrow',  mods = 'CTRL',        action = act.AdjustPaneSize { 'Down', 1 } },
  { key = 'LeftArrow',  mods = 'CTRL',        action = act.AdjustPaneSize { 'Left', 1 } },
  { key = 'RightArrow', mods = 'CTRL',        action = act.AdjustPaneSize { 'Right', 1 } },

  { key = 'UpArrow',    mods = 'CTRL|SHIFT',  action = act.AdjustPaneSize { 'Up', 5 } },
  { key = 'DownArrow',  mods = 'CTRL|SHIFT',  action = act.AdjustPaneSize { 'Down', 5 } },
  { key = 'LeftArrow',  mods = 'CTRL|SHIFT',  action = act.AdjustPaneSize { 'Left', 5 } },
  { key = 'RightArrow', mods = 'CTRL|SHIFT',  action = act.AdjustPaneSize { 'Right', 5 } },

  -- Open new splits with a command prompt
  {
    key = 'v',
    mods = 'LEADER',
    action = wezterm.action.PromptInputLine {
      description = 'Enter command for new vertical split:',
      action = wezterm.action_callback(function(window, pane, line)
        if line then
          window:active_pane():split{ direction = 'Down', args = { 'zsh', '-c', line } }
        end
      end),
    },
  },
  {
    key = 'h',
    mods = 'LEADER',
    action = wezterm.action.PromptInputLine {
      description = 'Enter command for new horizontal split:',
      action = wezterm.action_callback(function(window, pane, line)
        if line then
          window:active_pane():split{ direction = 'Right', args = { 'zsh', '-c', line } }
        end
      end),
    },
  },
}

-- Set vi-like keybindings for copy mode
config.key_tables = {
  copy_mode = {
    -- { key = 'v', mods = 'NONE', action = act.CopyMode 'StartSelection' },
    { key = 'y', mods = 'NONE', action = act.CopyTo 'ClipboardAndPrimarySelection' },
    { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
    { key = 'Enter', mods = 'NONE', action = act.CopyMode 'Close' },
  },
}

-- Set default command/shell, like `set -g default-command`

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    -- config.front_end = "Software" -- OpenGL doesn't work quite well with RDP.
    -- config.term = "" -- Set to empty so FZF works on windows
    config.default_prog = { 'pwsh.exe' }
    table.insert(config.launch_menu, { label = "PowerShell 7", args = {"pwsh.exe", "-NoLogo"} })
    table.insert(config.launch_menu, { label = "Windows PowerShell", args = {"powershell.exe", "-NoLogo"} })

    -- Find installed visual studio version(s) and add their compilation
    -- environment command prompts to the menu
    for _, vsvers in ipairs(wezterm.glob("Microsoft Visual Studio/20*", "C:/Program Files (x86)")) do
        local year = vsvers:gsub("Microsoft Visual Studio/", "")
        table.insert(config.launch_menu, {
            label = "x64 Native Tools VS " .. year,
            args = {"cmd.exe", "/k", "C:/Program Files (x86)/" .. vsvers .. "/BuildTools/VC/Auxiliary/Build/vcvars64.bat"},
        })
    end
else
    local fish_bin_path = "/bin/fish"
    if file_exists("/opt/homebrew/bin/fish") then
        fish_bin_path = "/opt/homebrew/bin/fish"
        config.default_prog = { '/opt/homebrew/bin/fish', '-l' }
    else
        config.default_prog = { '/bin/bash', '-l' }
    end
    table.insert(config.launch_menu, { label = "fish", args = {fish_bin_path, "-l"} })
    table.insert(config.launch_menu, { label = "bash", args = {"bash", "-l"} })
end

return config
