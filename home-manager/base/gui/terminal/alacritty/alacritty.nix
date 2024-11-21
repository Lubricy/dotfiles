{lib, ...}: {
  programs.alacritty = {
    enable = lib.mkDefault true;
    settings = {
      font = {
        size = lib.mkDefault 16;
        normal.family = "AnonymicePro Nerd Font Mono";
      };
      window = {
        padding = {
          x = lib.mkDefault 3;
        };
        dynamic_padding = true;
        decorations = "Buttonless";
      };
    };
  };
}
