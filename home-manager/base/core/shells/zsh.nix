{lib, ...}: let
  inherit (lib) mkMerge mkOrder;
  profileStart = mkOrder 100 ''
    if [[ -n "$PROFILE_STARTUP" ]]; then
      zmodload zsh/zprof
    fi
  '';
  profileEnd = mkOrder 9000 ''
    if [[ -n "$PROFILE_STARTUP" ]]; then
      zprof
    fi
  '';
in {
  programs.zsh.initContent = mkMerge [
    profileStart
    profileEnd
  ];
}
