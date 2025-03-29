{
  lib,
  config,
  ...
}: {
  imports = lib.dot.scanPaths ./.;
  options.features.wm.enable = lib.mkEnableOption "WM";
  config = lib.mkIf config.features.wm.enable {
    features.wm = {
      wallpaper.enable = true;
      notification.enable = true;
    };
  };
}
