{lib, mylib, pkgs, darwinConfig, ...}:
{
  modules.editors.emacs = {
    enable = true;
  };

  # language servers
  home.packages = with pkgs;[
    helm-ls
    nil
    nodePackages.prettier
    yaml-language-server
  ];
  home.activation.linkOrg = lib.hm.dag.entryBetween ["configBoundary"] ["installPackages"] (
    mylib.linkRepo {
      repo = {
        url = "https://github.com/Lubricy/mesb";
        name = "mesb";
      };
      hooks.postClone = ''
        git crypt unlock
      '';
    } darwinConfig.system.build.setEnvironment);
}
