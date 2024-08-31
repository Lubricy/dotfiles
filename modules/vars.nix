{lib, ...}: {
  options.vars = lib.mkOption {
    type = lib.types.raw;
    description = "custom vars";
    default = {};
  };
}
