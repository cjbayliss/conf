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

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };

    displayManager.startx.enable = true;
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

  environment.etc."X11/xinit/xinitrc".source = ./xinitrc;
  environment.etc."X11/xinit/xserverrc".source = lib.mkForce ./xserverrc;
  environment.etc."X11/xinit/xresources".source = ./xresources;

}
