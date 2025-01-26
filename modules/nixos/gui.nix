# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  pkgs,
  config,
  ...
}: {
  options.dot.features.gui.enable = lib.mkEnableOption "GUI";
  config = lib.mkIf config.dot.features.gui.enable {
    # TODO refactor this
    dot.features.nvidia.enable = true;
    # Select internationalisation properties.
    # i18n.defaultLocale = "en_US.UTF-8";
    # console = {
    #   font = "Lat2-Terminus16";
    #   keyMap = "us";
    # };

    # Enable the X11 windowing system.
    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;
    services.xserver.desktopManager.runXdgAutostartIfNone = true;
    # https://nixos.wiki/wiki/Wayland
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
    programs.hyprland = {
      enable = lib.mkDefault true;
      withUWSM = true;
    };

    # services.qemuGuest.enable = true;

    # Configure keymap in X11
    # services.xserver.layout = "us";
    # services.xserver.xkbOptions = "eurosign:e";

    # Enable CUPS to print documents.
    # services.printing.enable = true;

    # Enable sound.
    # sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # Enable touchpad support (enabled default in most desktopManager).
    services.libinput.enable = true;

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      alsa-utils # for pipewire control
      firefox
      wl-clipboard
      cliphist
    ];

    # enable fcitx5
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.waylandFrontend = true;
      fcitx5.addons = with pkgs; [
        fcitx5-material-color
        fcitx5-chinese-addons
        fcitx5-pinyin-zhwiki
        fcitx5-mozc
        fcitx5-gtk
      ];
    };
  };
}
