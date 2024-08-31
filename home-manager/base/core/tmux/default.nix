{lib, ...}: let
  launchTmux = ''
    if which tmux >/dev/null 2>&1; then
        #if not inside a tmux session, and if no session is started, start a new session
        test -z "$TMUX" && test -z "$SSH_CONNECTION" && test -z "$INSIDE_EMACS" && (tmux attach || tmux new-session)
    fi
  '';
in {
  programs.bash = {
    initExtra = launchTmux;
  };
  programs.zsh = {
    initExtra = launchTmux;
  };
  programs.tmux = {
    enable = true;
    prefix = "`";
    baseIndex = 1;
    keyMode = "vi";
    mouse = true;
    newSession = true;
    extraConfig = let
      dir = lib.dot.fromShared "tmux/tmux";
    in (with builtins;
      concatStringsSep "\n"
      (map
        (f: readFile (lib.path.append dir f))
        (filter
          (lib.strings.hasSuffix ".conf")
          (attrNames (readDir dir)))));
  };
}
