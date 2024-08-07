# HACK: add pam_reattach support until
# https://github.com/LnL7/nix-darwin/pull/591 merged
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.security.pam;

  # Implementation Notes
  #
  # We don't use `environment.etc` because this would require that the user manually delete
  # `/etc/pam.d/sudo` which seems unwise given that applying the nix-darwin configuration requires
  # sudo. We also can't use `system.patches` since it only runs once, and so won't patch in the
  # changes again after OS updates (which remove modifications to this file).
  #
  # As such, we resort to line addition/deletion in place using `sed`. We add a comment to the
  # added line that includes the name of the option, to make it easier to identify the line that
  # should be deleted when the option is disabled.
  mkSudoTouchIdAuthScript = isEnabled: let
    file = "/etc/pam.d/sudo";
    option = "security.pam.enableSudoTouchIdAuthReattach";
    sed = "${pkgs.gnused}/bin/sed";
  in ''
    ${
      if isEnabled
      then ''
        # Enable sudo Touch ID authentication, if not already enabled
        if ! grep 'pam_reattach.so' ${file} > /dev/null; then
          ${sed} -i '2i\
        auth       optional     ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh # nix-darwin: ${option}
          ' ${file}
        fi
      ''
      else ''
        # Disable sudo Touch ID authentication, if added by nix-darwin
        if grep '${option}' ${file} > /dev/null; then
          ${sed} -i '/${option}/d' ${file}
        fi
      ''
    }
  '';
in {
  config = {
    environment.systemPackages = [pkgs.pam-reattach];
    system.activationScripts.pam.text = mkAfter ''
      # PAM settings
      echo >&2 "setting up pam-reattach..."
      ${mkSudoTouchIdAuthScript cfg.enableSudoTouchIdAuth}
    '';
  };
}
