{
  lib,
  pkgs,
  ...
}: {
  modules.editors = {
    emacs.enable = true;
    lvim.enable = true;
  };

  # language servers
  home.packages = with pkgs; [
    helm-ls
    nil
    nodePackages.prettier
    yaml-language-server
  ];
}
