{myvars, pkgs, ...}:
let
  hostname = myvars.hostname;
in {
  networking.hostName = hostname;
  networking.computerName = hostname;
  system.defaults.smb.NetBIOSName = hostname;

  security.pki.certificateFiles =
    let systemCerts = pkgs.runCommand "dawin-system-ca-certificates.crt"
      {}
      ''
            /usr/bin/security find-certificate -a -p /System/Library/Keychains/SystemRootCertificates.keychain > "$out"
            /usr/bin/security find-certificate -a -p /Library/Keychains/System.keychain >> "$out"
          '';
    in [ systemCerts ];
}
