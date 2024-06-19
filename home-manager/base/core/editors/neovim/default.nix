{mylib, ...}: {
  programs = {
    neovim = {
      enable = true;

      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
    };
  };
  xdg.configFile."vim" = {
    source = mylib.fromShared "vim";
  };
}
