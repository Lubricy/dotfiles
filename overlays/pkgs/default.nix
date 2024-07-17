{lib, ...}:
self: super: {
  scriptkit = self.callPackage ./scriptkit.nix {};
  rancher-desktop = self.callPackage ./rancher-desktop.nix {};
}
