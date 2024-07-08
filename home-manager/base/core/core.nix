{
  lib,
  pkgs,
  dracula-sublime,
  ...
}: {
  home.packages = with pkgs; [
    # Misc
    tldr
    gnupg
    gnumake

    # Morden cli tools, replacement of grep/sed/...

    # Interactively filter its input using fuzzy searching, not limit to filenames.
    fzf
    # search for files by name, faster than find
    fd
    # search for files by its content, replacement of grep
    (ripgrep.override {withPCRE2 = true;})

    # A fast and polyglot tool for code searching, linting, rewriting at large scale
    # supported languages: only some mainstream languages currently(do not support nix/nginx/yaml/toml/...)
    ast-grep

    sad # CLI search and replace, just like sed, but with diff preview.
    yq-go # yaml processor https://github.com/mikefarah/yq

    # nix related
    #
    # it provides the command `nom` works just like `nix
    # with more details log output

    # productivity
  ];

  programs = {
    # A modern replacement for ‘ls’
    # useful in bash/zsh prompt, not in nushell.
    eza = {
      enable = true;
      # do not enable aliases in nushell!
      enableNushellIntegration = false;
      git = true;
      icons = true;
    };

    # a cat(1) clone with syntax highlighting and Git integration.
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        theme = "dracula";
      };
      themes = {
        dracula = {
          src = "${dracula-sublime}";
          file = "Dracula.tmTheme";
        };
      };
    };

    # A command-line fuzzy finder
    fzf = {
      enable = true;
    };

    # zoxide is a smarter cd command, inspired by z and autojump.
    # It remembers which directories you use most frequently,
    # so you can "jump" to them in just a few keystrokes.
    # zoxide works on all major shells.
    #
    #   z foo              # cd into highest ranked directory matching foo
    #   z foo bar          # cd into highest ranked directory matching foo and bar
    #   z foo /            # cd into a subdirectory starting with foo
    #
    #   z ~/foo            # z also works like a regular cd command
    #   z foo/             # cd into relative path
    #   z ..               # cd one level up
    #   z -                # cd into previous directory
    #
    #   zi foo             # cd with interactive selection (using fzf)
    #
    #   z foo<SPACE><TAB>  # show interactive completions (zoxide v0.8.0+, bash 4.4+/fish/zsh only)
    zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    # Atuin replaces your existing shell history with a SQLite database,
    # and records additional context for your commands.
    # Additionally, it provides optional and fully encrypted
    # synchronisation of your history between machines, via an Atuin server.
    atuin = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
      settings = {
        invert = true;
        inline_height = 10;
        workspaces = true;
        filter_mode_shell_up_key_binding = "directory";
        search_mode_shell_up_key_binding = "fulltext";
        update_check = false;
        style = "compact";
        prefers_reduced_motion = true;
        enter_accept = false;
        history_filter = [
          "^ls$"
          "^clear$"
        ];
      };
    };
  };
  # disable atuin ui, replace it with fzf
  programs.zsh.initExtra = lib.mkAfter ''
    _atuin_search() {
      local selected
      selected=$(atuin history list --cmd-only --session --print0 \
          | fzf --read0 --tac --query "$LBUFFER" \
            --exact \
            --no-sort \
            --ansi \
            --height=40% --margin=1 --padding=1 \
            --preview-window=up \
            --prompt 'Session> ' \
            --bind 'zero:transform:case "$FZF_PROMPT" in
                    "Session> ") echo -n "change-prompt(Dir> )+reload(atuin history list --cwd";;
                    "Dir> ")     echo -n "change-prompt(Global> )+reload(atuin history list";;
                    "Global> ")  echo -n "change-prompt(Session> )+reload(atuin history list --session";;
                    esac; echo " --cmd-only --print0)"
                  ' \
            --bind 'ctrl-r:transform:case "$FZF_PROMPT" in
                    "Session> ") echo -n "change-prompt(Dir> )+reload(atuin history list --cwd";;
                    "Dir> ")     echo -n "change-prompt(Global> )+reload(atuin history list";;
                    "Global> ")  echo -n "change-prompt(Session> )+reload(atuin history list --session";;
                    esac; echo " --cmd-only --print0)"
                  ' \
            --preview 'echo {} | bat -l zsh --color always -pp')

      if [ -n "$selected" ]; then
        BUFFER=$selected
        CURSOR=$#BUFFER
      fi
      zle reset-prompt
    }
    '';
}
