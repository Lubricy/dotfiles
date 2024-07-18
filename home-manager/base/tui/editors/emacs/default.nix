# ==============================================
# Based on doomemacs's auther's config:
#   https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
#
# Emacs Tutorials:
#  1. Official: <https://www.gnu.org/software/emacs/tour/index.html>
#  2. Doom Emacs: <https://github.com/doomemacs/doomemacs/blob/master/docs/index.org>
#
{
  config,
  lib,
  mylib,
  pkgs,
  doomemacs,
  ...
}@input:
with lib; let
  cfg = config.modules.editors.emacs;
  shellExtra = ''
    function e() {
        if [ -z "$INSIDE_EMACS" ]; then
            emacsclient --create-frame "$@"
        else
            emacsclient "$@"
        fi
    }
    function et() {
        if [ -z "$INSIDE_EMACS" ]; then
            emacsclient --create-frame --tty "$@"
        else
            emacsclient "$@"
        fi
    }
  '';

  myEmacsPackagesFor = emacs: ((pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: with epkgs; [
    vterm
    treesit-grammars.with-all-grammars
  ]));
in {
  options.modules.editors.emacs = {
    enable = mkEnableOption "Emacs Editor";
    doomConfig = mkOption {
      type= types.str;
      default = "https://github.com/Lubricy/dotfiles";
      description = "";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        ## Doom dependencies
        git
        (ripgrep.override {withPCRE2 = true;})
        gnutls # for TLS connectivity

        ## Optional dependencies
        fd # faster projectile indexing
        imagemagick # for image-dired
        fd # faster projectile indexing
        zstd # for undo-fu-session/undo-tree compression


        # go-mode
        # gocode # project archived, use gopls instead

        ## Module dependencies
        # :checkers spell
        (aspellWithDicts (ds: with ds; [en en-computers en-science]))
        # :tools editorconfig
        editorconfig-core-c # per-project style config
        # :tools lookup & :lang org +roam
        sqlite
        # :lang latex & :lang org (latex previews)
        (texlive.combine {
          inherit (texlive) scheme-medium tikz-cd;
        })
        # :lang org (jupyter)
        jupyter
      ];

      programs.bash.bashrcExtra = shellExtra;
      programs.zsh.initExtra = shellExtra;
      #programs.nushell.shellAliases = shellAliases;

      home.activation.installDoomEmacs = lib.hm.dag.entryAfter ["writeBoundary"] ''
        run ${pkgs.rsync}/bin/rsync -avz --chmod=D2755,F744 ${doomemacs}/ ${config.xdg.configHome}/emacs/
      '';
      # TODO find a way to inject path for syncDoomEmacs only
      # https://github.com/nix-community/home-manager/blob/master/modules/home-environment.nix
      home.emptyActivationPath = false;
      home.activation.configBoundary = lib.hm.dag.entryAfter ["installPackages"] "";
      home.sessionPath = [
        "${config.xdg.configHome}/emacs/bin"
      ];
    }

    (mkIf pkgs.stdenv.isLinux (
      let
        # Do not use emacs-nox here, which makes the mouse wheel work abnormally in terminal mode.
        # pgtk (pure gtk) build add native support for wayland.
        # https://www.gnu.org/savannah-checkouts/gnu/emacs/emacs.html#Releases
        emacsPkg = myEmacsPackagesFor pkgs.emacs29-pgtk;
      in {
        home.packages = [emacsPkg];
        services.emacs = {
          enable = true;
          package = emacsPkg;
          client = {
            enable = true;
            arguments = [" --create-frame"];
          };
          startWithUserSession = true;
        };
        home.activation.syncDoomEmacs = lib.hm.dag.entryAfter ["configBoundary"] ''
          export EMACS=${emacsPkg}/bin/emacs
          run ${config.xdg.configHome}/emacs/bin/doom sync
        '';
      }
    ))

    (mkIf pkgs.stdenv.isDarwin (
      let
        # macport adds some native features based on GNU Emacs 29
        # https://bitbucket.org/mituharu/emacs-mac/src/master/README-mac
        emacsPkg = myEmacsPackagesFor pkgs.emacsNoctuid;
      in {
        home.packages = [emacsPkg];
        launchd.enable = true;
        launchd.agents.emacs = {
          enable = true;
          config = {
            ProgramArguments = [
              "${pkgs.bash}/bin/bash"
              "-l"
              "-c"
              "${config.xdg.configHome}/emacs/bin/doom env && ${emacsPkg}/bin/emacs --fg-daemon"
            ];
            SoftResourceLimits.NumberOfFiles = 10240;
            StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/emacs-daemon.stderr.log";
            StandardOutPath = "${config.home.homeDirectory}/Library/Logs/emacs-daemon.stdout.log";
            RunAtLoad = true;
            KeepAlive = true;
          };
        };
        home.activation.syncDoomEmacs = lib.hm.dag.entryAfter ["configBoundary"] ''
          export EMACS=${emacsPkg}/bin/emacs
          run ${config.xdg.configHome}/emacs/bin/doom sync
        '';

        home.activation.installDoomConfig = lib.hm.dag.entryBetween ["configBoundary"] ["installPackages"] (
          mylib.linkRepo {
            repo = {
              url = cfg.doomConfig;
              name = "dotfiles";
            };
            mappings = {
              "shared/doom" = "${config.xdg.configHome}/doom";
            };
          } input.darwinConfig.system.build.setEnvironment);

        services.yabai = {
          extraConfig = lib.mkAfter "yabai -m rule --add title='doom-capture' manage=off grid=5:5:1:1:3:3";
        };
      }
    ))
  ]);
}
