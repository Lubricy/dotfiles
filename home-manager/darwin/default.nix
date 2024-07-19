{
  lib,
  mylib,
  myvars,
  darwinConfig,
  ...
}: {
  home.homeDirectory = "/Users/${myvars.username}";
  home.activation.createScreenshotsDir = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run mkdir -p ${darwinConfig.system.defaults.CustomUserPreferences."com.apple.screencapture".location}
  '';

  # modules.tools.open-interpreter.enable = true;
  imports =
    (mylib.scanPaths ./.)
    ++ [
      ../base/core
      ../base/tui
      ../base/gui
      ../base/home.nix
    ];
}
