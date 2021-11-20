{ config, pkgs, lib, ... }:

{
  # the bad
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "VCV-Rack"
      "b43-firmware"
      "bitwig-studio"
    ];

  boot.loader = {
    efi = { canTouchEfiVariables = false; };
    grub = {
      device = "nodev";
      efiInstallAsRemovable = true;
      efiSupport = true;
      font = null;
      forcei686 = false;
      splashImage = null;
    };
  };

  boot.kernelModules = [ "kvm-intel" "wacom" ];
  boot.blacklistedKernelModules = [ "isight_firmware" ];
  boot.kernelParams = [
    "acpi_mask_gpe=0x17"
    "video=SVIDEO-1:d"
    "systemd.restore_state=0"
    "threadirqs"
    "quiet"
  ];

  boot.postBootCommands = ''
    echo 2048 > /sys/class/rtc/rtc0/max_user_freq
    echo 2048 > /proc/sys/dev/hpet/max-user-freq
    ${pkgs.pciutils}/bin/setpci -v -d *:* latency_timer=b0
    ${pkgs.pciutils}/bin/setpci -v -s 00:1b.0 latency_timer=ff
  '';

  networking = {
    hostId = "163e24d6";
    hostName = "aster";

    enableB43Firmware = true;
    enableIPv6 = false;
    interfaces.ens5.useDHCP = true;
    interfaces.wlan0.useDHCP = true;
    wireless.interfaces = [ "wlan0" ];
    wireless.enable = true;

    resolvconf.enable = lib.mkDefault false;
    dhcpcd.extraConfig = "nohook resolv.conf";
  };

  services.xserver = {
    videoDrivers = [ "intel" ];

    deviceSection = ''
      Option "DRI" "2"
      Option "TearFree" "true"
    '';
  };

  system.stateVersion = "20.09";
}
