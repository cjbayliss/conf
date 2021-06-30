{ config, lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    layout = "us";

    videoDrivers = [ "intel" ];

    deviceSection = ''
      Option "DRI" "2"
      Option "TearFree" "true"
    '';

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm.enable = true;
      lightdm.greeters.mini = {
        enable = true;
        user = "cjb";
      };
    };
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
