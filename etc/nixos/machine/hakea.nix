{ config, pkgs, lib, ... }:

{
  imports = [
    # asus g14
    <nixos-hardware/asus/zephyrus/ga401>
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = [ "wacom" "acpi-call" ];
  boot.kernelParams = [ "threadirqs" "quiet" ];

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
      "nvidia-settings"
      "nvidia-x11"
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
    videoDrivers = [ "amdgpu" "nvidia" ];

    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
      Option         "TearFree" "true"
    '';
  };

  hardware.enableRedistributableFirmware = true;

  hardware.nvidia.modesetting.enable = true;
  powerManagement.powertop.enable = true;
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.powerManagement.finegrained = true;

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
