{
  self,
  nixpkgs,
  git-hooks,
  ...
} @ inputs: let
  inherit (inputs.nixpkgs) lib;

  allSystemNames = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
  root = path: import (self.lib.dot.relativeToRoot path);
  # Helper function to generate a set of attributes for each system
  forAllSystems = func: (lib.genAttrs allSystemNames func);
in {
  # Custom lib
  lib = import ../lib/extended-stdlib.nix {inherit lib inputs;};

  # NixOS Hosts
  nixosConfigurations = {};

  # macOS Hosts
  darwinConfigurations = {};

  nixosModules = {
    base = root "modules/common";
    default = root "modules/nixos";
    overlays = root "overlays";
  };
  darwinModules.default = root "modules/darwin";
  hmModules = rec {
    base = root "home-manager/base";
    nixos = root "home-manager/nixos";
    darwin = root "home-manager/darwin";
    default = base;
  };

  checks = forAllSystems (
    system: {
      pre-commit-check = git-hooks.lib.${system}.run {
        src = self.lib.dot.relativeToRoot ".";
        hooks = {
          alejandra.enable = true; # formatter
          # Source code spell checker
          typos = {
            enable = true;
            settings = {
              write = true; # Automatically fix typos
              exclude = ''"^$"''; # HACK: trigger `--force-exclude`
            };
          };
          prettier = {
            enable = true;
            settings = {
              write = true; # Automatically format files
            };
          };
          # deadnix.enable = true; # detect unused variable bindings in `*.nix`
          # statix.enable = true; # lints and suggestions for Nix code(auto suggestions)
        };
      };
    }
  );

  # Development Shells
  devShells = forAllSystems (
    system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        packages = with pkgs; [
          # fix https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
          bashInteractive
          # fix `cc` replaced by clang, which causes nvim-treesitter compilation error
          gcc
          # Nix-related
          alejandra
          deadnix
          statix
          nil
          # spell checker
          typos
          # code formatter
          nodePackages.prettier
        ];
        name = "dots";
        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}
          git config --local --unset-all core.hooksPath # HACK: prefer global hooksPath
        '';
      };
    }
  );

  templates = import ../templates {inherit lib;};

  # Format the nix code in this flake
  formatter = forAllSystems (
    # alejandra is a nix formatter with a beautiful output
    system: nixpkgs.legacyPackages.${system}.alejandra
  );
  packages = forAllSystems (system: {
    unfree = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
  });
  # Add attribute sets into outputs, for debugging
  # debugAttrs = {inherit nixosSystems darwinSystems allSystems allSystemNames;};
}
