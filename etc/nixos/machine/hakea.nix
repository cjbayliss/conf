{ config, pkgs, lib, ... }:

{
  imports = [
    <nixos-hardware/common/gpu/nvidia.nix>
    <nixos-hardware/common/cpu/amd>
    <nixos-hardware/common/pc/laptop>
    <nixos-hardware/common/pc/ssd>
    # hardening config
    ../harden.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = [ "wacom" ];
  boot.kernelParams = [ "threadirqs" "quiet" ];
  boot.blacklistedKernelModules = [ "nouveau" ];
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
      "nvidia-settings"
      "nvidia-x11"
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
    videoDrivers = [ "amdgpu" "nvidia" ];

    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
      Option         "TearFree" "true"
    '';
    libinput.mouse.accelSpeed = "1";
    libinput.mouse.accelProfile = "flat";
  };

  hardware.nvidia = {
    modesetting.enable = true;
    prime.offload.enable = true;
    prime.nvidiaBusId = "PCI:1:0:0";
    prime.amdgpuBusId = "PCI:4:0:0";
  };

  # fixes mic mute button
  services.udev.extraHwdb = ''
    evdev:name:*:dmi:bvn*:bvr*:bd*:svnASUS*:pn*:*
     KEYBOARD_KEY_ff31007c=f20
  '';

  # https://blog.nil.im/?7b
  systemd.tmpfiles.rules = [
    # only charge battery to 60%
    "w /sys/class/power_supply/BAT0/charge_control_end_threshold - - - - 60"
  ];

  powerManagement.powertop.enable = true;
  hardware.enableRedistributableFirmware = true;

  services.tlp.settings = {
    CPU_BOOST_ON_AC = 0;
    CPU_BOOST_ON_BAT = 0;
  };

  environment.sessionVariables = {
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_FONT_DPI = "144";
  };

  system.stateVersion = "22.05";

}
