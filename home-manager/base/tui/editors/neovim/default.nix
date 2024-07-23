{
  config,
  lib,
  pkgs,
  kickstart,
  ...
}:
###############################################################################
#
#  AstroNvim's configuration and all its dependencies(lsp, formatter, etc.)
#
#e#############################################################################
with lib;
let
  cfg = config.modules.editors.nvim;
  shellAliases = {
    v = "nvim";
    vdiff = "nvim -d";
  };
in {

  options.modules.editors.nvim = {
    enable = mkEnableOption "NeoVim Editor";
  };
  config = mkIf cfg.enable {
    xdg.configFile."nvim" = {
      source = kickstart;
    };

    home.shellAliases = shellAliases;
    #  programs.nushell.shellAliases = shellAliases;

    programs = {
      neovim = {
        enable = true;

        package = pkgs.unstable.neovim-unwrapped;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;

        # currently we use lazy.nvim as neovim's package manager, so comment this one.
        # Install packages that will compile locally or download FHS binaries via Nix!
        # and use lazy.nvim's `dir` option to specify the package directory in nix store.
        # so that these plugins can work on NixOS.
        #
        # related project:
        #  https://github.com/b-src/lazy-nix-helper.nvim
        plugins = with pkgs.vimPlugins; [
          # search all the plugins using https://search.nixos.org/packages
          telescope-fzf-native-nvim
        ];
      };
    };
  };
}
