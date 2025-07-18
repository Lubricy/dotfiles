{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    rancher-desktop
  ];
  home.sessionPath = [
    "${config.home.homeDirectory}/.rd/bin"
  ];
  programs.zsh.initContent = ''
    eval "$(rdctl completion zsh )"
    nerdctl completion zsh >/dev/null && eval "$(nerdctl completion zsh )"
  '';
}
