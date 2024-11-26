{
  lib,
  config,
  ...
}: {
  security.pam.services.${config.vars.username}.gnupg.enable = lib.mkDefault true;
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.settings = {
    default-cache-ttl = lib.mkDefault 43200;
    max-cache-ttl = lib.mkDefault 43200;
  };
}
