{nixpkgs-unstable, ...}: {
  nixpkgs.overlays = [
    (self: super: {
      unstable = import nixpkgs-unstable {
        inherit (super) overlays config;
        system = super.stdenv.hostPlatform.system;
      };
    })
  ];
}
