{lib, ...}: {
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../common
    ];
}
