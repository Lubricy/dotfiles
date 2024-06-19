{mylib,...}:
let
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
    initExtraFirst = builtins.readFile (mylib.fromShared "zshrc.d/my.zshtheme");
    initExtra = cd_ls;
  };

}
