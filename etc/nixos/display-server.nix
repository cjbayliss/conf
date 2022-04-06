{ config, lib, pkgs, ... }:

let unstable = import <nixos-unstable> { };
in {
  services.xserver = {
    enable = true;
    layout = "us";
    exportConfiguration = true;

    wacom.enable = true;
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      haskellPackages = unstable.haskellPackages;
    };

    displayManager.sx.enable = true;
  };

  services.picom.enable = true;

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
