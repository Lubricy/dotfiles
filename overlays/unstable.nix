{nixpkgs-unstable, ...}: (self: super: {
  unstable = import nixpkgs-unstable {
    inherit (super) system overlays;
  };
})
