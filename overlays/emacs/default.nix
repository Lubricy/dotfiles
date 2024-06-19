{lib,...}@args:
lib.lists.forEach [
  ./emacs.nix
  ./custom.nix
] (f: import f args)
