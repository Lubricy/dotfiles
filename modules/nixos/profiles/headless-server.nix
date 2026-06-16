{
  config,
  lib,
  ...
}: {
  config = lib.mkIf config.dot.profiles.headlessServer.enable {
    dot.features.fonts.enable = lib.mkForce false;

    programs.nix-index.enable = lib.mkForce false;
    programs.nix-index-database.comma.enable = lib.mkForce false;
    programs.dconf.enable = lib.mkForce false;

    documentation = {
      doc.enable = lib.mkDefault false;
      info.enable = lib.mkDefault false;
      man.enable = lib.mkDefault false;
      nixos.enable = lib.mkDefault false;
    };

    system.tools.nixos-option.enable = lib.mkForce false;

    fonts = {
      fontconfig.enable = lib.mkForce false;
      fontDir.enable = lib.mkForce false;
      packages = lib.mkForce [];
    };

    services.dbus.implementation = lib.mkDefault "dbus";
  };
}
