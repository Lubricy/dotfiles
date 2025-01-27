{
  lib,
  config,
  ...
}: {
  options.dotfiles = {
    url = lib.mkOption {
      type = lib.types.str;
      description = "dotfiles remote url";
      default = "https://github.com/Lubricy/dotfiles";
    };
    localPath = lib.mkOption {
      type = lib.types.str;
      description = "dotfiles remote url";
      default = "${config.xdg.configHome}/repos/dotfiles";
    };
  };
}
