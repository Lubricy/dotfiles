{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (lib) mkIf mkMerge;
in {
  config = mkIf config.modules.editors.emacs.enable (mkMerge [
    (
      mkIf pkgs.stdenv.isDarwin {
        services.yabai = {
          extraConfig = lib.mkAfter "yabai -m rule --add title='doom-capture' manage=off grid=5:5:1:1:3:3";
        };
      }
    )
  ]);
}
