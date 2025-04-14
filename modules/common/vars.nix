{
  lib,
  github-keys,
  ...
}: {
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
      authorizedKeyFiles = lib.mkOption {
        type = lib.types.listOf lib.types.path;
        default = [
          github-keys
        ];
      };
      authorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.singleLineStr;
        default = [
        ];
      };
    };
  };
}
