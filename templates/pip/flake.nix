{
  description = "Poetry Python Project";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # To import a flake module
        # 1. Add foo to inputs
        # 2. Add foo as a parameter to the outputs function
        # 3. Add here: foo.flakeModule

      ];
      systems = [
        # systems for which you want to build the `perSystem` attributes
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pythonPackages =  pkgs.python310Packages;
        in
          {
            # Per-system attributes can be defined here. The self' and inputs'
            # module parameters provide easy access to attributes of the same
            # system.

            # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
            devShells = {
              default = with pkgs; mkShell {
                venvDir = "./.venv";
                buildInputs = (with pythonPackages; [
                  python
                  venvShellHook
                ]);
                postVenvCreation = ''
                  unset SOURCE_DATE_EPOCH
                  pip install --upgrade pip
                  pip install -r requirements.txt
                '';
              };
            };
          };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

      };
    };
}
