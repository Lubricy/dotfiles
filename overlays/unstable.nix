{nixpkgs-unstable, ...}: {
  nixpkgs.overlays = [
    (self: super: {
      unstable = import nixpkgs-unstable {
        inherit (super) system overlays config;
      };
    })
  ];
}
