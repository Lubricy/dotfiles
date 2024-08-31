{
  lib,
  pkgs,
  ...
}: {
  programs.alacritty = {
    enable = lib.mkDefault true;
    settings = {
      font = {
        size = lib.mkDefault 16;
        normal.family = "AnonymicePro Nerd Font Mono";
      };
      window.decorations = "Buttonless";
    };
  };
}
