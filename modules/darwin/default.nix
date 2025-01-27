{lib, ...}: {
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../common
    ];
  dot.defaultUser.enable = true;
}
