{lib, ...}: {
  options.dot = {
    hostname = lib.mkOption {
      type = lib.types.str;
      description = "current hostname";
    };
    defaultUser = {
      enable = lib.mkEnableOption "Default User";
      username = lib.mkOption {
        type = lib.types.str;
        description = "username";
        default = "lubricy";
      };
    };
  };
}
