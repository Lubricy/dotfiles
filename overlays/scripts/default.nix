{lib, ...}: let
  dir = lib.dot.fromShared "bin";
  allFiles = lib.attrsets.filterAttrs (n: v: v == "regular") (builtins.readDir dir);
in {
  nixpkgs.overlays = [
    (self: super:
      lib.attrsets.mapAttrs
      (name: _: let
        inherit (lib) flatten filter foldl' concatMap hasAttr;
        inherit (lib.strings) match splitString;
        scriptPath = "${dir}/${name}";
        script = builtins.readFile scriptPath;
        regex = "#[[:space:]]*nix-depends?:[[:space:]]*(.*)";
        deplines = flatten (filter (i: i != null) (map (s: match regex s) (splitString "\n" script)));
        depstrs = filter (i: i != "") (concatMap (splitString " ") deplines);
        getDep = str:
          foldl'
          (p: s:
            if hasAttr s p
            then p.${s}
            else throw "attribute '${s}' missing in file '${scriptPath}'")
          self
          (splitString "." str);
        deps = map getDep depstrs;
      in
        super.writeShellApplication {
          name = name;
          runtimeInputs = [self.coreutils] ++ deps;
          text = script;
        })
      allFiles)
  ];
}
