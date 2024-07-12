{lib,...}:
with lib; with builtins;
concatMapAttrs (k: v:
  if v != "regular"
  then {
    "${k}" = {
      path = ./${k};
      description = "A template for generating ${k}.";
    };
  }
  else {}
) (readDir ./.)
