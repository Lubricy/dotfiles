{
  lib,
  config,
  ...
}: let
  cd_ls = ''
    foo_cd () {
      cd "$@";
      ls
    }
    alias cd=foo_cd

    if [ -d $HOME/.rc.d ]; then
      for file in $HOME/.rc.d/*; do
        source $file
      done
    fi
  '';
in {
  programs.bash = {
    initExtra = cd_ls;
  };
  programs.zsh = {
    initContent = lib.mkMerge [
      (lib.mkBefore (builtins.readFile (lib.dot.fromShared "zshrc.d/my.zshtheme")))
      cd_ls
    ];
  };

  home.sessionPath = [
    "${config.home.homeDirectory}/.local/bin"
  ];
}
