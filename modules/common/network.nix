{config, ...}: let
  hostname = config.dot.hostname;
in {
  networking.hostName = hostname;
}
