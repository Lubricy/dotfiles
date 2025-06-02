{
  lib,
  pkgs,
  config,
  ...
}: {
  options.dot.features.gui.enable = lib.mkEnableOption "GUI";
  config = lib.mkMerge [
    {programs.dconf.enable = true;}
    (lib.mkIf config.dot.features.gui.enable {
      # boot options
      boot = {
        kernelParams = [
          "quiet"
          "splash"
        ];
        plymouth = {
          enable = lib.mkDefault true;
          theme = "lone";
          themePackages = with pkgs; [
            # By default we would install all themes
            (adi1090x-plymouth-themes.override {
              selected_themes = ["lone"];
            })
          ];
        };
        # Enable "Silent Boot"
        consoleLogLevel = lib.mkDefault 0;
        initrd.verbose = lib.mkDefault false;
        # Set timeout to 0 hide the OS choice for bootloaders.
        # It's *not* possible to open the bootloader list in GRUB
        # so set to 1 instead
        loader.timeout = 1;
      };
      # TODO refactor this
      dot.features.nvidia.enable = lib.mkDefault true;
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
    })
  ];
}
