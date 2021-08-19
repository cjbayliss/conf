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

    displayManager.startx.enable = true;
    desktopManager.plasma5.enable = true;
    desktopManager.xterm.enable = false;
  };

  environment.etc."X11/xinit/xinitrc".source = ./xinitrc;
  environment.etc."X11/xinit/xserverrc".source = lib.mkForce ./xserverrc;
  environment.etc."X11/xinit/xresources".source = ./xresources;

}
