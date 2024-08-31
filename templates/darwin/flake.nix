{
  description = "nix flake for darwin";

  outputs = {dotfiles, ...}: let
    hostname = "<change-me>";
    username = "<change-me>";
    system = "aarch64-darwin"; # optional: change-me
  in {
    darwinConfigurations.${hostname} = dotfiles.lib.dot.macosSystem {
      vars = {inherit hostname username system;};
      darwin-modules = [
        ./configuration.nix
      ];
      home-modules = [
        dotfiles.hmModules.darwin
        ./home.nix
      ];
    };
  };
}
