{config, ...}: let
  hostname = config.vars.hostname;
in {
  networking.hostName = hostname;
}
