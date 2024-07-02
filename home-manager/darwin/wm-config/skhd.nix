{
  config,
  mylib,
  myvars,
  ...
}: {
  services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile (mylib.fromShared "skhd/skhdrc");
  };
}
