{lib, ...}: {
  home.activation.linkOrg = lib.hm.dag.entryBetween ["configBoundary"] ["installPackages"] (
    lib.dot.linkRepo {
      repo = {
        url = "https://github.com/Lubricy/mesb";
        name = "mesb";
      };
      hooks.postClone = ''
        git crypt unlock
      '';
    }
  );
}
