{lib, ...}: {
  options.vars.authorizedKeys = lib.mkOption {
    type = lib.types.listOf lib.types.singleLineStr;
    default = [
    ];
  };
}
