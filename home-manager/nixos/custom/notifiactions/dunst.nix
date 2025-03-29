{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkForce;
  finalConfig = "${config.xdg.configHome}/dunst/matugen";
  colorConfig = "${config.xdg.configHome}/dunst/colors.toml";
  # TODO: revisit this when `dunstctl reload` is released
  dunstPkg = pkgs.dunst.overrideAttrs (old: {
    version = "git"; # usually harmless to omit
    src = pkgs.fetchFromGitHub {
      owner = "dunst-project";
      repo = "dunst";
      rev = "423c3a1a1a9ec96cb9d7a9e76d641f12c7e5eada";
      sha256 = "sha256-drpaAApHH7T0QVqqzhExmXVn6vY25tuvVz+Gw1Nq0lE=";
    };
    postInstall = ''
      wrapProgram $out/bin/dunst \
        --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"

      wrapProgram $out/bin/dunstctl \
        --prefix PATH : "${lib.makeBinPath (with pkgs; [coreutils dbus])}"
      substituteInPlace $out/share/zsh/site-functions/_dunstctl $out/share/fish/vendor_completions.d/{dunstctl,dunstify}.fish \
            --replace-fail "jq" "${lib.getExe pkgs.jq}"

    '';
  });
  cfg = config.modules.notifications.dunst;
in {
  options.modules.notifications.dunst = {
    enable = mkEnableOption "dunst notification service";
  };

  config = mkIf cfg.enable {
    services.dunst = {
      enable = true;
      package = dunstPkg;

      configFile = finalConfig;
      settings = {
        # experimental.per_monitor_dpi = false;
        global = {
          monitor = 0;
          follow = "mouse";
          width = 300;
          height = 60;
          offset = "(30, 50)";
          origin = "top-right";
          transparency = 10;
          progress_bar = true;
          progress_bar_height = 10;
          progress_bar_frame_width = 1;
          progress_bar_min_width = 150;
          progress_bar_max_width = 300;
          indicate_hidden = true;
          separator_height = 2;
          padding = 8;
          horizontal_padding = 8;
          text_icon_padding = 0;
          frame_width = 1;
          sort = true;
          line_height = 0;
          markup = "full";
          format = "<b>%s</b>\\n%b";
          alignment = "left";
          vertical_alignment = "center";
          show_age_threshold = 60;
          ellipsize = "middle";
          ignore_newline = false;
          stack_duplicates = true;
          hide_duplicate_count = false;
          show_indicators = true;
          icon_position = "left";
          min_icon_size = 0;
          max_icon_size = 32;
          sticky_history = true;
          history_length = 50;
          browser = "${pkgs.xdg-utils}/bin/xdg-open";
          always_run_script = true;
          corner_radius = 8;
          ignore_dbusclose = false;
          force_xwayland = false;
          force_xinerama = false;
          mouse_left_click = "do_action, close_current";
          mouse_right_click = "close_current";
          mouse_middle_click = "close_all";
          separator_color = "frame";
        };
        urgency_low = {
          timeout = 5;
        };

        urgency_normal = {
          timeout = 30;
        };

        urgency_critical = {
          timeout = 0;
        };
      };
    };

    xdg.configFile."dunst/config".onChange = mkForce ''
      cat ${colorConfig} ${config.xdg.configHome}/dunst/config > ${finalConfig}
      ${dunstPkg}/bin/dunstctl reload ${finalConfig}
    '';
  };
}
