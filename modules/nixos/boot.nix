{
  lib,
  pkgs,
  ...
}: {
  services.lvm.enable = lib.mkDefault true;
  boot = {
    tmp.useTmpfs = lib.mkDefault true;
    initrd.services.lvm.enable = lib.mkDefault true;
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
    consoleLogLevel = 0;
    initrd.verbose = false;
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
    ];
    # Hide the OS choice for bootloaders.
    # It's still possible to open the bootloader list by pressing any key
    # It will just not appear on screen unless a key is pressed
    loader.timeout = 0;
    # Use the systemd-boot EFI boot loader.
    # boot.loader.systemd-boot.enable = lib.mkDefault true;
    # boot.loader.efi.canTouchEfiVariables = lib.mkDefault true;

    # Use the grub EFI boot loader.
    loader.grub = {
      enable = lib.mkDefault true;
      device = "nodev";
      efiSupport = lib.mkDefault true;
      useOSProber = lib.mkDefault true;
      extraGrubInstallArgs = ["--modules=part_gpt part_msdos lvm ext2"];
    };
    loader.efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = lib.mkDefault true;
    };
  };
}
