{ config, pkgs, ... }:

with pkgs;
let
  chromium = (ungoogled-chromium.override {
    commandLineArgs = ''
      $([ $(date "+%k") -ge 17 ] || [ $(date "+%k") -le 5 ] && echo "--force-dark-mode --enable-features=WebUIDarkMode")'';
  });
  mpv = (mpv-with-scripts.override { scripts = [ mpvScripts.mpris ]; });
  python = python3.withPackages (pp: with pp; [ flake8 notify2 pylint ]);
  emacs = (pkgs.emacsPackagesGen pkgs.emacsGit).emacsWithPackages (epkgs:
    with epkgs; [
      elfeed
      emms
      haskell-mode
      marginalia
      nix-mode
      php-mode
      pinentry
      tree-sitter
      tree-sitter-langs
    ]);
in
{

  imports = [
    # hardware
    ./hardware-configuration.nix
    ./machine/default.nix
    # display server config
    ./display-server.nix
    # hardening config
    ./harden.nix
  ];

  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;

  services.resolved = {
    enable = true;
    dnssec = "true";
    extraConfig = ''
      DNS=1.1.1.1
      DNSOverTLS=yes
    '';
    fallbackDns = [ "1.0.0.1" ];
  };

  environment.etc."issue".enable = false;

  time.timeZone = "Australia/Melbourne";

  i18n.defaultLocale = "en_AU.UTF-8";
  console = {
    font = "";
    keyMap = "us";
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  users.users.cjb = {
    isNormalUser = true;
    extraGroups = [ "audio" "wheel" "video" ];
  };

  environment.systemPackages = with pkgs; [
    # libs
    aspell
    aspellDicts.en
    browserpass
    dmenu
    dunst
    git-filter-repo
    hsetroot
    mangohud
    opusTools
    pciutils
    pulseaudio # for pactl
    sx
    universal-ctags
    xmobar
    yaru-theme

    # langs
    chicken
    fennel
    gcc
    ghc
    php74
    python
    sbcl

    # langs-extras
    black
    fnlfmt
    hlint
    nix-linter
    nixpkgs-fmt
    proselint
    shellcheck

    # tools
    beets
    crudini
    efibootmgr
    feh
    ffmpeg
    git
    imagemagick
    pandoc
    pass
    playerctl
    protonup
    scrot
    unzip
    w3m
    wget
    winePackages.stagingFull
    winetricks
    yt-dlp

    # plugins/synths/drums
    distrho
    helm
    surge
    zyn-fusion

    # gui
    bitwig-studio
    chromium
    discord
    emacs
    firefox
    j4-dmenu-desktop
    krita
    mpv
    vcv-rack

    (pkgs.writeTextFile {
      name = "set-gtk-theme";
      destination = "/bin/set-gtk-theme";
      executable = true;
      text = ''
        #!/bin/sh

        [ $(date "+%k") -ge 17 ] || [ $(date "+%k") -le 5 ] && \
          crudini --set $HOME/.config/gtk-3.0/settings.ini Settings gtk-application-prefer-dark-theme true || \
          crudini --set $HOME/.config/gtk-3.0/settings.ini Settings gtk-application-prefer-dark-theme false
      '';
    })

    (pkgs.writeTextFile {
      name = "emacs-askpass";
      destination = "/bin/emacs-askpass";
      executable = true;
      text = ''
        #! ${pkgs.bash}/bin/bash
        emacsclient -e '(read-passwd "Password: ")' | xargs
      '';
    })
  ];

  # FIXME: learn how to use systemd timers
  services.cron.enable = true;
  programs.ssh.askPassword = "emacs-askpass";

  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
  };
  security.pam.loginLimits = [
    {
      domain = "@audio";
      item = "memlock";
      type = "-";
      value = "unlimited";
    }
    {
      domain = "@audio";
      item = "rtprio";
      type = "-";
      value = "99";
    }
    {
      domain = "@audio";
      item = "nofile";
      type = "soft";
      value = "99999";
    }
    {
      domain = "@audio";
      item = "nofile";
      type = "hard";
      value = "99999";
    }
  ];

  services.udev.extraRules = ''
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';

  programs.light.enable = true;
  programs.ssh.startAgent = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "emacs";
  };

  virtualisation.podman.enable = true;

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
      baekmuk-ttf
      inter
      iosevka-bin
      ipafont
      liberation_ttf
      noto-fonts-emoji
      tenderness
    ];
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
      defaultFonts.monospace =
        [ "Iosevka Fixed" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      defaultFonts.sansSerif = [
        "Inter"
        "Liberation Sans"
        "IPAGothic"
        "Baekmuk Gulim"
        "Noto Color Emoji"
      ];
      defaultFonts.serif = [
        "Tenderness"
        "Liberation Serif"
        "IPAGothic"
        "Baekmuk Gulim"
        "Noto Color Emoji"
      ];
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
    loginShellInit = ''
      if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] ;
        then exec sx;
      fi
    '';
  };

  # variables set by PAM on login
  environment.sessionVariables = {
    XDG_DESKTOP_DIR = "$HOME/stuff/desktop";
    XDG_DOCUMENTS_DIR = "$HOME/stuff";
    XDG_DOWNLOAD_DIR = "$HOME/downloads";
    XDG_MUSIC_DIR = "$HOME/music";
    XDG_PICTURES_DIR = "$HOME/pictures";
    XDG_VIDEOS_DIR = "$HOME/videos";

    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "/tmp/cache";
    XDG_DATA_HOME = "$HOME/.local/share";

    PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/startup.py";
    PASSWORD_STORE_DIR = "$XDG_DATA_HOME/pass";
    MOZ_GTK_TITLEBAR_DECORATION = "system"; # proper theming
    MOZ_USE_XINPUT2 = "1";

    EDITOR = "emacsclient";
    VISUAL = "emacsclient";

    # IMPORTANT: without these, audio plugins installed via nix are not found
    DSSI_PATH =
      "/run/current-system/sw/lib/dssi:~/.nix-profile/lib/dssi:~/.dssi";
    LADSPA_PATH =
      "/run/current-system/sw/lib/ladspa:~/.nix-profile/lib/ladspa:~/.ladspa";
    LV2_PATH = "/run/current-system/sw/lib/lv2:~/.nix-profile/lib/lv2:~/.lv2";
    LXVST_PATH =
      "/run/current-system/sw/lib/lxvst:~/.nix-profile/lib/lxvst:~/.lxvst";
    VST_PATH = "/run/current-system/sw/lib/vst:~/.nix-profile/lib/vst:~/.vst";
    VST3_PATH =
      "/run/current-system/sw/lib/vst3:~/.nix-profile/lib/vst3:~/.vst3";

    # FIXME: make this *only* get set for 'cjb'
    EMAIL = "cjb@cjb.sh";
    NAME = "Christopher Bayliss";
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

}
