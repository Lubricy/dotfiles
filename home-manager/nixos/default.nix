{lib, ...}: {
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../base
    ];
}
