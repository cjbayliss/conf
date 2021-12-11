{ config, pkgs, lib, ... }:

{
  imports = [
    <nixos-hardware/common/cpu/amd>
    <nixos-hardware/common/pc/laptop>
    <nixos-hardware/common/pc/ssd>
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = [ "wacom" ];
  boot.kernelParams = [ "threadirqs" "quiet" ];
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
  boot.cleanTmpDir = true;

  # for low latency audio
  boot.postBootCommands = ''
    echo 2048 > /sys/class/rtc/rtc0/max_user_freq
    echo 2048 > /proc/sys/dev/hpet/max-user-freq
    ${pkgs.pciutils}/bin/setpci -v -d *:* latency_timer=b0 >/dev/null
    ${pkgs.pciutils}/bin/setpci -v -s 04:00.6 latency_timer=ff >/dev/null
  '';

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "VCV-Rack"
      "bitwig-studio"
      "discord"
      "steam"
      "steam-original"
      "steam-runtime"
    ];

  networking = {
    hostId = "f822b5b3";
    hostName = "hakea";

    enableIPv6 = false;
    interfaces.wlp2s0.useDHCP = true;
    wireless.interfaces = [ "wlp2s0" ];
    wireless.enable = true;

    resolvconf.enable = lib.mkDefault false;
    dhcpcd.extraConfig = "nohook resolv.conf";
  };

  services.xserver = {
    videoDrivers = [ "amdgpu" ];

    screenSection = ''
      Option         "TripleBuffer" "on"
      Option         "TearFree" "true"
    '';
  };

  # fixes mic mute button
  services.udev.extraHwdb = ''
    evdev:name:*:dmi:bvn*:bvr*:bd*:svnASUS*:pn*:*
     KEYBOARD_KEY_ff31007c=f20
  '';

  # https://blog.nil.im/?7b
  systemd.tmpfiles.rules = [
    # disable nvidia GPU
    "w /sys/bus/pci/devices/0000:01:00.0/remove - - - - 1"
    "w /sys/bus/pci/devices/0000:01:00.1/remove - - - - 1"
    # set fan profile to silent
    "w /sys/devices/platform/asus-nb-wmi/throttle_thermal_policy - - - - 2"
    # only charge battery to 60%
    "w /sys/class/power_supply/BAT0/charge_control_end_threshold - - - - 60"
  ];

  powerManagement.powertop.enable = true;
  hardware.enableRedistributableFirmware = true;
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];
  };

  programs.steam.enable = true;

  services.tlp.settings = {
    CPU_BOOST_ON_AC = 0;
    CPU_BOOST_ON_BAT = 0;
  };

  environment.sessionVariables = {
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_FONT_DPI = "144";
  };

  system.stateVersion = "21.11";

}
