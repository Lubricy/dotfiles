{lib, ...}: {
  options.vars = {
    hostname = lib.mkOption {
      type = lib.types.str;
      description = "current hostname";
    };
    username = lib.mkOption {
      type = lib.types.str;
      description = "username";
      default = "lubricy";
    };
    system = lib.mkOption {
      type = lib.types.enum lib.systems.flakeExposed;
      description = "current system";
    };
  };
}
