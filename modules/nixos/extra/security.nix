{
  lib,
  config,
  ...
}: {
  config = lib.mkMerge [
    {
      programs.gnupg.agent.enable = true;
      programs.gnupg.agent.settings = {
        default-cache-ttl = lib.mkDefault 43200;
        max-cache-ttl = lib.mkDefault 43200;
        # required by pam
        allow-preset-passphrase = "";
      };
    }
    (lib.mkIf config.dot.defaultUser.enable {
      security.pam.services.${config.dot.defaultUser.username}.gnupg.enable = lib.mkDefault true;
    })
  ];
}
