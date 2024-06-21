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
  pname = "scriptkit";
  # Update hashes for both Linux and Darwin!
  version = "2.3.0";

  sources = {
    x86_64-linux = fetchurl {
      url = "https://github.com/johnlindquist/kitapp/releases/download/v${version}/Kit-Linux-${version}-x86_64.AppImage";
      sha256 = "sha256-TUDs8ezVSC1VhGO/cQQY/dlyAzR1bf+KKCtO7ZRukrg=";
    };

    aarch64-linux = fetchurl {
      url = "https://github.com/johnlindquist/kitapp/releases/download/v${version}/Kit-Linux-${version}-arm64.AppImage";
      sha256 = "sha256-4hZlBIUO+wnAvtke0Lf06PxiKC/IoYI4yjcSOJ/fcWo=";
    };
    x86_64-darwin = fetchurl {
      url = "https://github.com/johnlindquist/kitapp/releases/download/v${version}/Kit-macOS-${version}-x64.dmg";
      sha256 = "sha256-qlZedlFDxHAJDa/XE3O4UzHQ0qnYRAnuCG1HuBIBFbA=";
    };
    aarch64-darwin = fetchurl {
      url = "https://github.com/johnlindquist/kitapp/releases/download/v${version}/Kit-macOS-${version}-arm64.dmg";
      sha256 = "sha256-9TnMCJdbiJA7s1gclxLqt9Gk/oRWCdVd0+jMHfjJUkU=";
    };
  };

  meta = with lib; {
    license = licenses.mit;
    homepage = "https://github.com/johnlindquist/kitapp";
    description = "Script Kit Launcher";
    longDescription = "Script Kit. Automate Anything.";
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

    nativeBuildInputs = [ undmg ];
    sourceRoot = ".";

    installPhase = ''
      mkdir -p $out/Applications/
      cp -a *.app $out/Applications/
    '';
  }
