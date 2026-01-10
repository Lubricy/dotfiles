{
  lib,
  pkgs,
  ...
}: {
  # enable fcitx5
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5.waylandFrontend = true;
    fcitx5.addons = with pkgs; [
      fcitx5-material-color
      qt6Packages.fcitx5-chinese-addons
      fcitx5-pinyin-zhwiki
      fcitx5-mozc
      fcitx5-gtk
    ];
  };
  xdg.configFile."fcitx5".source = lib.dot.fromShared "fcitx5";
}
