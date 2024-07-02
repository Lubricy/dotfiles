{
  fzf-tab,
  zsh-vi-mode,
  ...
}:
let
  shellAliases = {
    k = "kubectl";
    cat = "bat";

    urldecode = "python3 -c 'import sys, urllib.parse as ul; print(ul.unquote_plus(sys.stdin.read()))'";
    urlencode = "python3 -c 'import sys, urllib.parse as ul; print(ul.quote_plus(sys.stdin.read()))'";
  };
in {
  # only works in bash/zsh, not nushell
  home.shellAliases = shellAliases;

  programs.bash = {
    enable = true;
    enableCompletion = true;
  };
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    plugins = [
      {
        name = "fzf-tab";
        file = "fzf-tab.plugin.zsh";
        src = "${fzf-tab}";
      }
      {
        name = "zsh-vi-mode";
        file = "zsh-vi-mode.plugin.zsh";
        src = "${zsh-vi-mode}";
      }
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "kubectl" ];
    };
  };
}
