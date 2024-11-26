{
  description = "nix flake for nixos";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/24.11";
  inputs.nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.dotfiles = {
    url = "github:Lubricy/dotfiles";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nixpkgs-unstable.follows = "nixpkgs-unstable";
  };

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
      # home-modules = [
      #   dotfiles.hmModules.nixos
      #   ./home.nix
      # ];
    };
  };
}
