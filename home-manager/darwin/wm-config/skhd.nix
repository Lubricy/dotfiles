{lib, ...}: {
  services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile (lib.dot.fromShared "skhd/skhdrc");
  };
}
