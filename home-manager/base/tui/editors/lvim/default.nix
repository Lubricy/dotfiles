{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption types mkIf;
  cfg = config.modules.editors.lvim;
  shellAliases = {
    v = "nvim";
    vdiff = "nvim -d";
  };
in {
  options.modules.editors.lvim = {
    enable = mkEnableOption "LunarVim Editor";
    viAlias = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Symlink {command}`vi` to {command}`lvim` binary.
      '';
    };

    vimAlias = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Symlink {command}`vim` to {command}`lvim` binary.
      '';
    };
    nvimAlias = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Symlink {command}`nvim` to {command}`lvim` binary.
      '';
    };
    defaultEditor = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to configure {command}`lvim` as the default
        editor using the {env}`EDITOR` environment variable.
      '';
    };
  };
  config = mkIf cfg.enable {
    home.shellAliases = shellAliases;
    home.sessionVariables = mkIf cfg.defaultEditor {EDITOR = "lvim";};

    #  programs.nushell.shellAliases = shellAliases;
    home.packages = [
      (pkgs.lunarvim.override {
        inherit (cfg) viAlias vimAlias nvimAlias;
      })
    ];
    home.activation.fetchDotfilesForLvim = lib.hm.dag.entryAfter ["configBoundary"] ''
      __dotfileRepoPath="${config.dotfiles.localPath}"
      if [ -d "$__dotfileRepoPath" ]; then
        echo "$__dotfileRepoPath exists. Skipping..."
      else
        git clone "${config.dotfiles.url}" "$__dotfileRepoPath"
      fi
    '';
    xdg.configFile = lib.dot.linkShared config ["lvim"];
  };
}
