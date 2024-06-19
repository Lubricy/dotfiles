{
  pkgs,
  ...
}:
let kubectl-ctx = pkgs.kubectx.overrideAttrs(old: {
    postInstall = old.postInstall + ''
      cp ${old.src}/kubectx $out/bin/kubectl-ctx
      cp ${old.src}/kubens $out/bin/kubectl-ns
      chmod +x $out/bin/kubectl-{ns,ctx}
    '';
  });
  in {
  home.packages = with pkgs; [
    kubectl
    kubectl-tree
    kubectl-ctx

  ];

  programs = {
  };
}
