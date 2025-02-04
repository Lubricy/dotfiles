{lib, ...}: let
  inherit (lib) concatMap attrsToList hasSuffix;
  path = ./.;
  files = attrsToList (builtins.readDir path);
  filter = {
    name,
    value,
  }:
    if name == "default.nix"
    then []
    else if value == "directory" || hasSuffix ".nix" name
    then ["${path}/${name}"]
    else [];
in {
  imports = concatMap filter files;
}
