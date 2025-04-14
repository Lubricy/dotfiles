{lib, ...}: {
  services.lvm.enable = lib.mkDefault true;
  boot = {
    tmp.useTmpfs = lib.mkDefault true;
    initrd.services.lvm.enable = lib.mkDefault true;

    kernelParams = [
      "boot.shell_on_fail"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
    ];
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
      configurationLimit = lib.mkDefault 10;
    };
    loader.efi = {
      efiSysMountPoint = "/boot/efi";
      canTouchEfiVariables = lib.mkDefault true;
    };
  };
}
