{
  lib,
  config,
  ...
}: {
  options.vars = {
    dotfilesUrl = lib.mkOption {
      type = lib.types.str;
      description = "dotfiles remote url";
      default = "https://github.com/Lubricy/dotfiles";
    };
    dotfilesLocalPath = lib.mkOption {
      type = lib.types.str;
      description = "dotfiles remote url";
      default = "${config.xdg.configHome}/repos/dotfiles";
    };
  };
}
