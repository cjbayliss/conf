{ config, pkgs, lib, ... }:

with pkgs;
let
  unstable = import <unstable> {};
  chromium = (ungoogled-chromium.override {
    commandLineArgs = ''$([ $(date "+%k") -ge 17 ] || [ $(date "+%k") -le 5 ] && echo "--force-dark-mode --enable-features=WebUIDarkMode")'';
  });
  mpv = (mpv-with-scripts.override {
    scripts = [ mpvScripts.mpris ];
  });
  python = python3.withPackages (pp: with pp; [
    notify2
  ]);
  emacs = (pkgs.emacsPackagesGen pkgs.emacsGit).emacsWithPackages (
    epkgs: [
      epkgs.elpaPackages.marginalia
      epkgs.elpaPackages.pinentry
      epkgs.melpaPackages.haskell-mode
      epkgs.melpaPackages.nix-mode
      epkgs.melpaPackages.php-mode
      epkgs.melpaPackages.tree-sitter
      epkgs.melpaPackages.tree-sitter-langs
    ]
  );
in
{
  # the bad
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "b43-firmware"
  ];

  imports =
    [
      # hardware
      ./hardware-configuration.nix
      # display server config
      ./display-server.nix
      # hardening config
      ./harden.nix
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
    wireless.interfaces = [ "wlan0" ];
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

  environment.etc."issue".enable = false;

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
    extraGroups = [ "audio" "wheel" "video" ];
  };

  environment.systemPackages = with pkgs; [
    # libs
    aspell
    aspellDicts.en
    browserpass
    dunst
    git-filter-repo
    gnome3.adwaita-icon-theme
    libnotify
    opusTools
    pciutils
    universal-ctags

    # langs
    chicken
    ghc
    php73
    python
    sbcl

    # tools
    beets
    black
    cryptsetup
    ed
    efibootmgr
    feh
    ffmpeg
    gcc
    git
    hlint
    htop
    imagemagick
    pass
    playerctl
    unstable.neovim
    unzip
    wget
    youtube-dl

    # gui
    chromium
    emacs
    firefox
    gimp
    mpv
    rofi
    xfce.terminal

    # extras
    (pkgs.writeScriptBin "volctl" (builtins.readFile ./bin/volctl))
    (pkgs.writeTextFile {
      name = "startx";
      destination = "/bin/startx";
      executable = true;
      text = ''
         #!${pkgs.bash}/bin/bash
         xinit /etc/X11/xinit/xinitrc -- vt$(tty | tail -c2)
       '';
    })
    (pkgs.writeTextFile {
      name = "set-audio-latency";
      destination = "/bin/set-audio-latency";
      executable = true;
      text = ''
        #! ${pkgs.bash}/bin/bash

        echo 2048 > /sys/class/rtc/rtc0/max_user_freq
        echo 2048 > /proc/sys/dev/hpet/max-user-freq
        ${pkgs.pciutils}/bin/setpci -v -d *:* latency_timer=b0
        ${pkgs.pciutils}/bin/setpci -v -s 00:1b.0 latency_timer=ff
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

  programs.ssh.askPassword = "emacs-askpass";

  environment.etc = {
    "chromium/native-messaging-hosts/com.github.browserpass.native.json".source =
      "${pkgs.browserpass}/lib/browserpass/hosts/chromium/com.github.browserpass.native.json";
  };

  sound.enable = true;
  # for some reason linux tries to use a swap even if you don't have
  # one, causing audio to skip/crackle really badly.
  boot.kernel.sysctl."vm.swappiness" = 0;

  # while the above fixes the worst of the audio skipping I've
  # experianced under NixOS, the following it an attempt to reduce audio
  # skipping when the system has high load. See:
  # https://nixos.wiki/wiki/JACK
  systemd.services.setup-audio-latency = {
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "/run/current-system/sw/bin/set-audio-latency";
    };
    wantedBy = [ "multi-user.target" ];
  };
  security.pam.loginLimits = [
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
    { domain = "@audio"; item = "nofile"; type = "soft"; value = "99999"; }
    { domain = "@audio"; item = "nofile"; type = "hard"; value = "99999"; }
  ];
  services.udev = {
    extraRules = ''
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
    '';
  };

  programs.light.enable = true;
  programs.ssh.startAgent = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "emacs";
  };

  virtualisation.podman.enable = true;

  fonts = {
    fonts = with pkgs; [
      baekmuk-ttf
      iosevka-bin
      ipafont
      liberation_ttf
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
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
    interactiveShellInit = ''
      # better history
      shopt -s histappend
      HISTCONTROL=ignoreboth
      HISTSIZE=-1
      HISTFILESIZE=-1
      HISTFILE="$XDG_DATA_HOME/bash-history"

      # check the window size after each command and, if necessary, update
      # the values of LINES and COLUMNS.
      shopt -s checkwinsize

      # better completion (imo)
      bind "set menu-complete-display-prefix on"
      bind "set show-all-if-ambiguous on"
      bind "set completion-query-items 0"
      bind "TAB:menu-complete"
      bind "\"\e[Z\": menu-complete-backward"
    '';
    promptInit = ''
      __exit_status() {
          EXIT_STATUS=''${?}
          if [ $EXIT_STATUS -gt 0 ]; then
              printf "%s " "$EXIT_STATUS"
          fi
      }

      __short_cwd() {
          CWD="$(basename $PWD)"
          SHORT_PWD_NO_CWD="$(echo $PWD | sed -e 's/\/usr\/home\/'$USER'/~/' -e 's/\/home\/'$USER'/~/' -e 's|'$CWD'$||')"
          COLLAPSED_PWD="$(echo $SHORT_PWD_NO_CWD | sed -r 's|/(.)[^/]*|/\1|g')"
          [ "$SHORT_PWD_NO_CWD" == "~" ] && \
              SHORT_CWD="$SHORT_PWD_NO_CWD" || \
                  SHORT_CWD="$COLLAPSED_PWD$CWD"
          printf "%s" "$SHORT_CWD"
      }

      __git_branch() {
          BRANCH="$(git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/')"
          printf "%s" "$BRANCH"
      }

      __git_prompt() {
          [ "$(git ls-files 2>/dev/null | wc -l)" -lt 2000 ] && \
              STATUS="$(git status --short 2>/dev/null | sed 's/^ //g' | cut -d' ' -f1 | sort -u | tr -d '\n' | sed 's/^/ /')" || STATUS=" [NOSTAT]"
          printf "%s" "$STATUS"
      }

      PS1="\[\e[1;31m\]\$(__exit_status)\[\e[0m\]\h \[\e[1;34m\]::\[\e[0m\] \[\e[32m\]\$(__short_cwd)\[\e[0m\]\[\e[33m\]\$(__git_branch)\[\e[0m\]\$(__git_prompt)\[\e[0m\] \[\e[1;34m\]»\[\e[0m\] "
    '';
    loginShellInit = ''
      if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] ;
        then exec startx;
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
    MOZ_USE_XINPUT2 = "1";

    # FIXME: make this *only* get set for 'cjb'
    EMAIL = "cjb@cjb.sh";
    NAME = "Christopher Bayliss";
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/29829efa3367108398a024db927263d95a55bba2.tar.gz;
    }))
  ];

  system.stateVersion = "20.09";
}
