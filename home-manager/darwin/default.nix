{
  lib,
  config,
  darwinConfig,
  ...
}: {
  home.homeDirectory = "/Users/${config.vars.username}";
  home.activation.createScreenshotsDir = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run mkdir -p ${darwinConfig.system.defaults.CustomUserPreferences."com.apple.screencapture".location}
  '';

  # modules.tools.open-interpreter.enable = true;
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../base
    ];
}
