{ config, pkgs, lib, ...}:

{
  imports = [
    # asus g14
    <nixos-hardware/asus/zephyrus/ga401>
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelModules = [ "wacom" ];
  boot.kernelParams = [
    "threadirqs"
    "quiet"
  ];

  # for low latency audio
  boot.postBootCommands = ''
    echo 2048 > /sys/class/rtc/rtc0/max_user_freq
    echo 2048 > /proc/sys/dev/hpet/max-user-freq
    ${pkgs.pciutils}/bin/setpci -v -d *:* latency_timer=b0
    ${pkgs.pciutils}/bin/setpci -v -s 04:00.6 latency_timer=ff
  '';

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "VCV-Rack"
      "bitwig-studio"
      "nvidia-x11"
      "nvidia-settings"
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

  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.powerManagement.finegrained = true;

  services.picom.enable = true;
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];
  };

  system.stateVersion = "21.11";

}
