{
  lib,
  pkgs,
  ...
}:
{
  programs.alacritty = {
    enable = true;
    settings = {
      font= {
        size = 16;
        normal.family = "AnonymicePro Nerd Font Mono";
      };
      window.decorations = "Buttonless";
    };
  };
}
