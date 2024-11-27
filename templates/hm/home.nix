{
  lib,
  config,
  ...
}: {
  imports = [
    ./modules
  ];
  # home.sessionVariables = {
  #   KUBECONFIG = "$(find ${config.xdg.configHome}/kube -type f -name 'config.*' | paste -sd: -)";
  # };
  # home.packages = with pkgs;[ cowsay ];
  modules.editors.emacs.enable = true;

  # xdg.configFile."pip/pip.conf".text = ''
  #   [global]
  #   extra-index-url = https://pypi.tuna.tsinghua.edu.cn/simple
  #   extra-index = https://pypi.tuna.tsinghua.edu.cn/simple
  #   format = columns
  # '';
}
