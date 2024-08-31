{
  system,
  nixpkgs-unstable,
  ...
}: (self: super: {
  unstable = import nixpkgs-unstable {
    inherit system;
    overlays = super.overlays;
  };
})
