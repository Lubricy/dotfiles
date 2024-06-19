{lib,...}:
with lib builtins;
concatMapAttrs (k: v:
  if v != "regular"
  then {
    "${v}" = let desc = "./${k}/README.md";in {
      path = "./${k}";
      description = if pathExists desc
                    then readFIle desc
                    else "A template for generating ${k}.";
    };
  }
  else {}
) (readDir ./.)
