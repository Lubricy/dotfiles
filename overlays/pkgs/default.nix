{lib,...}@args:
lib.lists.forEach (
  (builtins.filter # find all overlay files in the current directory

  (
    f:
      f
      != "default.nix" # ignore default.nix
      && f != "README.md" # ignore README.md
  )
  (builtins.attrNames (builtins.readDir ./.)))
) (f: self: super:
  let pkg = super.callPackage f {};
  in {
    ${pkg.pname} = pkg;
  })
