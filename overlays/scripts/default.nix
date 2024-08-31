{lib, ...}: let
  dir = lib.dot.fromShared "bin";
  allFiles = lib.attrsets.filterAttrs (n: v: v == "regular") (builtins.readDir dir);
in
  self: super:
    lib.attrsets.mapAttrs
    (name: _:
      super.writeShellScriptBin name (builtins.readFile "${dir}/${name}"))
    allFiles
