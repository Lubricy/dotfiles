{lib, pkgs,...}:
self: super: {
  scriptkit = self.callPackage ./scriptkit.nix {};
}
