{
  lib,
  config,
  ...
}: {
  # xdg.configFile."pip/pip.conf".text = ''
  #   [global]
  #   extra-index-url = https://pypi.tuna.tsinghua.edu.cn/simple
  #   extra-index = https://pypi.tuna.tsinghua.edu.cn/simple
  #   format = columns
  # '';
  home.sessionVariables = {
    # KUBECONFIG = "$(find ${config.xdg.configHome}/kube -type f -name 'config.*' | paste -sd: -)";
  };
  modules.editors.emacs.enable = true;
  # home.packages =
  #   let
  #     scriptsDir = ./scripts;
  #     scripts = with builtins;
  #       map (f: pkgs.writeShellScriptBin f (readFile (scriptsDir + "/${f}")))
  #         (attrNames (readDir scriptsDir));
  #     packages = with pkgs;[
  #       argocd
  #       kubeseal
  #     ];
  #   in scripts ++ packages;
  #home.activation.linkOrg = lib.hm.dag.entryBetween ["configBoundary"] ["installPackages"] (
  #  lib.dot.linkRepo {
  #    repo = {
  #      url = "https://github.com/Lubricy/mesb";
  #      name = "mesb";
  #    };
  #    hooks.postClone = ''
  #      git crypt unlock
  #    '';
  #  }
  #);
}
