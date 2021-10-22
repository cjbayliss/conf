{ config, lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    layout = "us";
    exportConfiguration = true;
    videoDrivers = [ "intel" ];

    deviceSection = ''
      Option "DRI" "2"
      Option "TearFree" "true"
    '';

    wacom.enable = true;
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    displayManager.startx.enable = true;
    desktopManager.xterm.enable = false;
  };

  services.redshift = {
    enable = true;
    temperature.day = 5000;
    temperature.night = 4000;
  };

  location = {
    latitude = -38.0;
    longitude = 145.0;
  };

}
