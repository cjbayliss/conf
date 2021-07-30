{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "ahci" "firewire_ohci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "wacom" ];
  boot.blacklistedKernelModules = [ "isight_firmware" ];
  boot.kernelParams = [
    "acpi_mask_gpe=0x17"
    "video=SVIDEO-1:d"
    "systemd.restore_state=0"
    "quiet"
  ];

  fileSystems."/" =
    {
      device = "rpool/root";
      fsType = "zfs";
    };

  fileSystems."/home" =
    {
      device = "rpool/home";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/41E4-F990";
      fsType = "vfat";
    };

  swapDevices = [ ];

}
