{lib, mylib, config, darwinConfig, ...}:
{
  modules.editors.emacs = {
    enable = true;
  };

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
