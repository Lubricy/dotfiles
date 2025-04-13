{pkgs, ...}: {
  # enable fcitx5
  i18n.inputMethod = {
    # enable = true;
    # type = "fcitx5";
    enabled = "fcitx5";
    # FIXME: waylandFrontend does not exist as of home-manager
    # release-24.11, but defined in master
    # fcitx5.waylandFrontend = true;
    fcitx5.addons = with pkgs; [
      fcitx5-material-color
      fcitx5-chinese-addons
      fcitx5-pinyin-zhwiki
      fcitx5-mozc
      fcitx5-gtk
    ];
  };
}
