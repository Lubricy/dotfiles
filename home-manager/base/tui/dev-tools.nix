{
  pkgs,
  pkgs-unstable,
  mylib,
  ...
}: {
  #############################################################
  #
  #  Basic settings for development environment
  #
  #  Please avoid to install language specific packages here(globally),
  #  instead, install them:
  #     1. per IDE, such as `programs.neovim.extraPackages`
  #     2. per-project, using https://github.com/the-nix-way/dev-templates
  #
  #############################################################

  home.packages = with pkgs; [

    # db related
    sqlite

    # ai related
    pkgs-unstable.python312Packages.huggingface-hub # huggingface-cli
    nodejs

    # misc
    watch
    entr

    jq
    # Automatically trims your branches whose tracking remote refs are merged or gone
    # It's really useful when you work on a project for a long time.
  ];

  home.file.".jq".source = mylib.fromShared "jq";

  programs = {
    direnv = {
      enable = true;
      config = {
        global = {
          # Hides the rather large block of text that is usually printed when entering the environment.
          hide_env_diff = true;
        };
      };
      nix-direnv.enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };
  };
}
