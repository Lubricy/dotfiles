{config, ...}: {
  home.homeDirectory = "/home/${config.vars.username}";
  programs.alacritty.settings.font.size = 12;
}
