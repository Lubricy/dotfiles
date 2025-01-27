{
  lib,
  config,
  ...
}: {
  config = lib.mkIf config.dot.defaultUser.enable {
    users.users.${config.dot.defaultUser.username}.extraGroups = ["dialout" "input"];
  };
}
