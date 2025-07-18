{pkgs, ...}: let
  # The final package is defined here so we can reference it recursively.
  nixos-spec-pkg = pkgs.writeShellApplication rec {
    name = "nixos-spec";
    text = builtins.readFile ./nixos-spec.sh;
    derivationArgs = {
      postInstall = ''
        installShellCompletion \
          --cmd "${name}" \
          --bash "${completions/_comp.bash}" \
          --zsh "${completions/_comp.zsh}" \
        ;
      '';
    };
  };
  # This is what will be installed in the system environment;
in {
  environment.systemPackages = [nixos-spec-pkg];
}
