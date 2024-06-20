{mylib, ...}: {
  xdg.configFile."vim".source = mylib.fromShared "vim";

  # TODO: remove this when vim 9.1 supports $XDG_CONFIG_HOME
  home.file.".vim".source = mylib.fromShared "vim";
}
