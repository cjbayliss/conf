{ config, pkgs, lib, ... }:

with pkgs;
let
  firefoxWithPassFFHost = (firefox-esr.override {
    extraNativeMessagingHosts = [ passff-host ];
  });
in
{
  nixpkgs.config.allowUnfree = true; # 😭
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader = {
    efi = {
      canTouchEfiVariables = false;
    };
    grub = {
      device = "nodev";
      efiInstallAsRemovable = true;
      efiSupport = true;
      font = null;
      forcei686 = false;
      splashImage = null;
    };
  };

  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;

  networking = {
    enableIPv6 = false;
    hostId = "163e24d6";
    hostName = "aster";
    useDHCP = true;
    resolvconf.enable = lib.mkDefault false;
    dhcpcd.extraConfig = "nohook resolv.conf";
  };

  services.resolved = {
    enable = true;
    dnssec = "true";
    extraConfig = ''
      DNS=1.1.1.1
      DNSOverTLS=yes
'';
    fallbackDns = [ "1.0.0.1" ];
  };

  time.timeZone = "Australia/Melbourne";

  i18n.defaultLocale = "en_AU.UTF-8";
  console = {
    font = "";
    keyMap = "us";
  };

  powerManagement.powertop.enable = true;
  powerManagement.cpuFreqGovernor = "schedutil";

  users.users.cjb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" ];
  };

  environment.systemPackages = with pkgs; [
    aria2
    aspell
    aspellDicts.en
    beets
    black
    cryptsetup
    efibootmgr
    emacsPgtk
    ffmpeg
    firefoxWithPassFFHost
    git
    git-filter-repo
    gnome3.adwaita-icon-theme
    imagemagick
    kakoune
    mpv
    opusTools
    pass
    pinentry-qt
    python3
    redshift
    tealdeer
    unzip
    wget
    youtube-dl

    # create executables I want
    (pkgs.writeTextFile {
      name = "startw";
      destination = "/bin/startw";
      executable = true;
      text = ''
        #! ${pkgs.bash}/bin/bash

        export EDITOR="emacsclient";
        export VISUAL="emacsclient";

        export EMAIL="cjb@cjb.sh"
        export NAME="Christopher Bayliss"

        export PASSWORD_STORE_DIR="$HOME/.local/share/pass"

        # set QT theme engine, requires qt5ct 😑
        export QT_QPA_PLATFORMTHEME="qt5ct"

        # wayland stuff
        export GDK_BACKEND=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export SDL_VIDEODRIVER=wayland
        export XDG_CURRENT_DESKTOP=sway
        export XDG_SESSION_TYPE=wayland

        # XDG_*_DIRs
        export XDG_DESKTOP_DIR="$HOME/stuff/desktop"
        export XDG_DOCUMENTS_DIR="$HOME/stuff"
        export XDG_DOWNLOAD_DIR="$HOME/downloads"
        export XDG_MUSIC_DIR="$HOME/music"
        export XDG_PICTURES_DIR="$HOME/pictures"
        export XDG_VIDEOS_DIR="$HOME/videos"

        # more XDG_* stuff
        export XDG_CONFIG_HOME="$HOME/.config"
        # why store this? put it in /tmp
        export XDG_CACHE_HOME="/tmp/cache"
        export XDG_DATA_HOME="$HOME/.local/share"

        # ensure $XDG_*_HOME exists
        mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

        # use emacs-askpass
        export SSH_ASKPASS="emacs-askpass"

        # imput method
        export XMODIFIERS=@im=ibus
        export CLUTTER_IM_MODULE=ibus
        export GTK_IM_MODULE=ibus
        export QT_IM_MODULE=ibus

        # set redshift to fix my screen's whitebalance
        redshift -m drm -x -O 4800

        # start wayland compositor
        systemctl --user import-environment
        ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway
      '';})

    (pkgs.writeTextFile {
      name = "emacs-askpass";
      destination = "/bin/emacs-askpass";
      executable = true;
      text = ''
        #! ${pkgs.bash}/bin/bash
        emacsclient -e '(read-passwd "Password: ")' | xargs
      '';})
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      grim
      slurp
      swayidle
      swaylock
    ];
  };

  sound.enable = true;

  programs.light.enable = true;
  programs.ssh.startAgent = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

  fonts = {
    fonts = with pkgs; [
      baekmuk-ttf
      ipafont
      liberation_ttf
      iosevka-bin
      noto-fonts-emoji
    ];
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
      defaultFonts.monospace = [ "Iosevka Fixed" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      defaultFonts.sansSerif = [ "Liberation Sans" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      defaultFonts.serif = [ "Liberation Serif" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      useEmbeddedBitmaps = true;

      localConf = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <!-- AFAICT NixOS doesn't provide a way to set hintstyle -->
          <match target="font">
            <edit name="hintstyle" mode="assign">
              <const>hintfull</const>
            </edit>
          </match>

          <!-- ensure Emacs chooses the 'Regular' weight, not the 'Medium' one. -->
          <selectfont>
            <rejectfont>
              <pattern>
                <patelt name="family" >
                  <string>Iosevka Fixed</string>
                </patelt>
                <patelt name="weight" >
                  <const>medium</const>
                </patelt>
              </pattern>
            </rejectfont>
          </selectfont>
        </fontconfig>

      '';
    };
  };

  gtk.iconCache.enable = true;
  programs.bash = {
    shellInit = "export HISTFILE=/dev/null";
    promptInit = ''
      if [ "$UID" == 0 ]; then
          PS1="\w # "
      else
          PS1="\w $ "
      fi
    '';
  };

  environment.variables = {
    EDITOR = "kak";
    VISUAL = "kak";
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  system.stateVersion = "20.09";
}
