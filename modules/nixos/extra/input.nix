{pkgs, ...}: {
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5.waylandFrontend = true;
    fcitx5.addons = with pkgs; [
      fcitx5-material-color
      fcitx5-chinese-addons
      fcitx5-pinyin-zhwiki
      fcitx5-mozc
      fcitx5-gtk
    ];
  };
}
