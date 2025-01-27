{
  impermanence,
  config,
  ...
}: {
  imports = [
    impermanence.nixosModules.impermanence
  ];
  fileSystems."/nix/persist/system" = {
    device = "/dev/disk/by-uuid/e0c975e2-69b3-478c-815b-58cb1ec1530e";
    fsType = "btrfs";
    neededForBoot = true;
  };
  environment.persistence."/nix/persist/system" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/etc/NetworkManager/system-connections"
      "/nix/var"
      "/var/log"
      "/var/lib"
    ];
    files = [
      "/etc/machine-id"
    ];
  };
  users.mutableUsers = false;
  users.users."${config.dot.defaultUser.username}" = {
    hashedPassword = "$6$NxW09BRd4KuH/sN.$epqwVur43u6DGpxbUooJc2.TfLqoFaB7bEbZkzX1vq9YXFpc25.rFbCV4GCXtvbkuBFwEBk8U9L/8cztti5Ih1";
  };
}
