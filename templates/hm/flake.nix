{
  description = "Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and dotfiles.
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles = {
      url = "github:Lubricy/dotfiles";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    dotfiles,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    username = "<change-me>"; # NOTE: should match current user
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      lib = dotfiles.lib;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [
        dotfiles.nixosModules.overlays
        dotfiles.hmModules.nixos
        {home.username = username;}
        ./home.nix
      ];
      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
      extraSpecialArgs = dotfiles.inputs // inputs;
    };
    packages.${system}.default = home-manager.packages.${system}.default;
  };
}
