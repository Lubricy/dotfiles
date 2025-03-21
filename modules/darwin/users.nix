{
  lib,
  config,
  ...
}: let
  cfg = config.dot.defaultUser;
in {
  # Define a user account. Don't forget to set a password with ‘passwd’.
  config = lib.mkIf cfg.enable {
    users.users."${cfg.username}" = {
      home = "/Users/${cfg.username}";

      # set user's default shell back to zsh
      #    `chsh -s /bin/zsh`
      # DO NOT change the system's default shell to nushell! it will break some apps!
      # It's better to change only kitty/wezterm's shell to nushell!
    };
  };
}
