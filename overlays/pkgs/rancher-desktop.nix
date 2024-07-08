{
  fetchurl,
  stdenv,
  lib,
  appimageTools,
  undmg,
  system,
  ...
}:

let
  pname = "rancher-desktop";
  # Update hashes for both Linux and Darwin!
  version = "1.14.2";

  sources = {
    x86_64-darwin = fetchurl {
      url = "https://github.com/rancher-sandbox/rancher-desktop/releases/download/v${version}/Rancher.Desktop-${version}.x86_64.dmg";
      sha256 = "sha256-52TjNdFHX4vOs/ttHRiSsJ08K+4/NDVd2rin4VfIdFI=";
    };
    aarch64-darwin = fetchurl {
      url = "https://github.com/rancher-sandbox/rancher-desktop/releases/download/v${version}/Rancher.Desktop-${version}.aarch64.dmg";
      sha256 = "sha256-/M+EM34KiUzaBH6hxXluTehDqfmSZl0oqUUeEJcGG/w=";
    };
  };

  meta = with lib; {
    license = licenses.asl20;
    homepage = "https://github.com/rancher-sandbox/rancher-desktop";
    description = "Container Management and Kubernetes on the Desktop";
    longDescription = ''
      Rancher Desktop is an open-source project that brings Kubernetes and container management to the desktop.
      It runs on Windows, macOS and Linux.
    '';
    platforms = lib.attrsets.attrNames sources;
  };

  passthru = {
    inherit sources;
  };

in
if stdenv.isLinux
then
  appimageTools.wrapType2 {
    inherit pname version passthru meta;
    src = sources."${system}";
  }
else
  stdenv.mkDerivation {
    inherit pname version passthru meta;

    src = sources."${system}";
    unpackCmd = ''
        echo "File to unpack: $curSrc"
        if ! [[ "$curSrc" =~ \.dmg$ ]]; then return 1; fi
        mnt=$(mktemp -d -t ci-XXXXXXXXXX)

        function finish {
          echo "Detaching $mnt"
          /usr/bin/hdiutil detach $mnt -force
          rm -rf $mnt
        }
        trap finish EXIT

        echo "Attaching $mnt"
        /usr/bin/hdiutil attach -nobrowse -readonly $src -mountpoint $mnt

        echo "What's in the mount dir"?
        ls -la $mnt/

        echo "Copying contents"
        shopt -s extglob
        DEST="$PWD"
        (cd "$mnt"; cp -a !(Applications) "$DEST/")
      '';
    sourceRoot = ".";
    dontMakeSourcesWritable = true;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/Applications/Rancher Desktop.app"
      cp -a "./Rancher Desktop.app/." "$out/Applications/Rancher Desktop.app/"
    '';

  }
