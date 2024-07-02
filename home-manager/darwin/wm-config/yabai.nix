{
  config,
  myvars,
  mylib,
  ...
}: let
  homeDir = config.users.users."${myvars.username}".home;
in {
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/by-name/ya/yabai/package.nix
  services.yabai = {
    enable = true;

    # Whether to enable yabai's scripting-addition.
    # SIP must be disabled for this to work.
    # https://github.com/koekeishiya/yabai/wiki/Disabling-System-Integrity-Protection
    # enableScriptingAddition = false;
    # config = {};
    extraConfig = builtins.readFile (mylib.fromShared "yabai/yabairc");
  };

  # custom log path for debugging
}
