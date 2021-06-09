{ config, pkgs, lib, ... }:

with pkgs;
let
  firefoxWithPassFFHost = (firefox.override {
    extraNativeMessagingHosts = [ passff-host ];
  });
  emacs = (pkgs.emacsPackagesGen pkgs.emacsPgtkGcc).emacsWithPackages (
    epkgs: [
      epkgs.elpaPackages.emms
      epkgs.elpaPackages.modus-themes
      epkgs.melpaPackages.circe
      epkgs.melpaPackages.elfeed
      epkgs.melpaPackages.elpher
      epkgs.melpaPackages.erc-hl-nicks
      epkgs.melpaPackages.gcmh
      epkgs.melpaPackages.highlight-numbers
      epkgs.melpaPackages.nix-mode
      epkgs.melpaPackages.php-mode
      epkgs.melpaPackages.rust-mode
    ]);
in
{
  # the bad
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "b43-firmware"
  ];

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
  boot.tmpOnTmpfs = true;

  networking = {
    hostId = "163e24d6";
    hostName = "aster";

    enableB43Firmware = true;
    enableIPv6 = false;
    interfaces.ens5.useDHCP = true;
    interfaces.wlan0.useDHCP = true;
    wireless.enable = true;

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
    aspell
    aspellDicts.en
    beets
    black
    breeze-qt5
    breeze-icons
    chicken
    cryptsetup
    ed
    efibootmgr
    emacs
    ffmpeg
    firefoxWithPassFFHost
    git
    git-filter-repo
    gnome3.adwaita-icon-theme
    imagemagick
    mpv
    opusTools
    pass
    pinentry-qt
    python3
    qt5ct
    redshift
    sbcl
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

        # set QT theme engine, requires qt5ct 😑
        export QT_QPA_PLATFORMTHEME="qt5ct"

        # wayland stuff
        export GDK_BACKEND=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export SDL_VIDEODRIVER=wayland
        export XDG_CURRENT_DESKTOP=sway
        export XDG_SESSION_TYPE=wayland

        # use emacs-askpass
        export SSH_ASKPASS="emacs-askpass"

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

  programs.fish = {
    enable = true;
    shellInit = "set fish_greeting";
    loginShellInit = ''
      set -gx XDG_DESKTOP_DIR "$HOME/stuff/desktop"
      set -gx XDG_DOCUMENTS_DIR "$HOME/stuff"
      set -gx XDG_DOWNLOAD_DIR "$HOME/downloads"
      set -gx XDG_MUSIC_DIR "$HOME/music"
      set -gx XDG_PICTURES_DIR "$HOME/pictures"
      set -gx XDG_VIDEOS_DIR "$HOME/videos"

      set -gx XDG_CONFIG_HOME "$HOME/.config"
      set -gx XDG_CACHE_HOME "/tmp/cache"
      set -gx XDG_DATA_HOME "$HOME/.local/share"
      mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

      set -gx PYTHONSTARTUP "$XDG_CONFIG_HOME/python/startup.py"
      set -gx PASSWORD_STORE_DIR "$XDG_DATA_HOME/pass"

      set -gx EMAIL "cjb@cjb.sh"
      set -gx NAME "Christopher Bayliss"
    '';
  };
  users.users.cjb.shell = pkgs.fish;

  environment.variables = {
    EDITOR = "ed";
    VISUAL = "ed";
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/8e63b4865f49636c9d990d5946d3d8db132536ec.tar.gz;
    }))
  ];

  system.stateVersion = "20.09";
}
