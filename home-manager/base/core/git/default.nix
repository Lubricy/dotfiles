{
  config,
  lib,
  pkgs,
  myvars,
  mylib,
  ...
}: {
  # `programs.git` will generate the config file: ~/.config/git/config
  # to make git use this config file, `~/.gitconfig` should not exist!
  #
  #    https://git-scm.com/docs/git-config#Documentation/git-config.txt---global
  home.activation.removeExistingGitconfig = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
    rm -f ${config.home.homeDirectory}/.gitconfig
  '';

  home.packages = with pkgs; [
    git-credential-pass
    difftastic
    lucky-commit
    gitleaks
    git-open
    git-crypt
    git-trim
  ];

  xdg.configFile."git/hooks" = {
    source = mylib.fromShared "git/hooks";
  };

  programs.git = {
    enable = true;
    lfs.enable = true;

    userName = "Lubricy Fibber";
    userEmail = "lubricy@gmail.com";

    ignores = lib.splitString "\n" (lib.fileContents (mylib.fromShared "git/global-gitignore"));

    includes = [
      { path = "${config.xdg.configHome}/git/local/config"; }
    ];

    extraConfig = {
      core.hooksPath = "${config.xdg.configHome}/git/hooks";
      credential.helper = "pass";
      init.defaultBranch = "main";
      trim.bases = "develop,master,main"; # for git-trim
      push.autoSetupRemote = true;
      pull.rebase = true;
      diff.tool = "difftastic";
      difftool."difftastic".cmd = "${pkgs.difftastic}/bin/difft";
    };
  };
}
