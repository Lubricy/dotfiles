{
  description = "nix flake for nixos";
  inputs.nixpkgs.url = flake:nixpkgs;
  inputs.dotfiles.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {dotfiles, ...} @ inputs: let
    hostname = "<change-me>";
    username = "<change-me>";
    system = "x86_64-linux"; # optional: change me
  in {
    nixosConfigurations.${hostname} = dotfiles.lib.dot.nixosSystem {
      vars = {inherit hostname username system;};
      specialArgs = inputs;
      nixos-modules = [
        ./configuration.nix
      ];
      home-modules = [
        dotfiles.hmModules.nixos
        ./home.nix
      ];
    };
  };
}
