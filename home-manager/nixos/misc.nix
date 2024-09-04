{config, ...}: {
  home.homeDirectory = "/home/${config.vars.username}";
  programs.alacritty.settings.font.size = 12;
  home.sessionVariables = {
    XCURSOR_THEME = "Adwaita"; # HACK: required by alacritty
  };
}
