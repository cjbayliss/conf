{ config, pkgs, lib, ... }:

with pkgs;
let
  unstable = import <unstable> {};
  chromium = (ungoogled-chromium.override {
    commandLineArgs = ''$([ $(date "+%k") -ge 17 ] && echo "--force-dark-mode --enable-features=WebUIDarkMode")'';
  });
  mpvWithMpris = (mpv-with-scripts.override {
    scripts = [ mpvScripts.mpris ];
  });
  emacs = (pkgs.emacsPackagesGen pkgs.emacsGit).emacsWithPackages (
    epkgs: [
      epkgs.elpaPackages.marginalia
      epkgs.elpaPackages.pinentry
      epkgs.melpaPackages.haskell-mode
      epkgs.melpaPackages.nix-mode
      epkgs.melpaPackages.php-mode
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
    extraGroups = [ "wheel" "video" ];
  };

  environment.systemPackages = with pkgs; [
    # libs
    aspell
    aspellDicts.en
    browserpass
    git-filter-repo
    gnome3.adwaita-icon-theme
    opusTools
    universal-ctags

    # langs
    chicken
    ghc
    php73
    python3
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
    mpvWithMpris
    rofi
    xfce.terminal

    # extras
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

  programs.light.enable = true;
  programs.ssh.startAgent = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "emacs";
  };

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

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    histSize = 20000;
    histFile = "$XDG_DATA_HOME/zsh/history";
    ohMyZsh.enable = true;
    ohMyZsh.theme = "afowler";
    shellInit = ''
      mkdir -p $XDG_DATA_HOME/zsh/
      export ZDOTDIR="$XDG_DATA_HOME/zsh/"

      set -o inc_append_history
      set -o hist_ignore_all_dups
      set -o hist_ignore_space

      # yeah yeah, this is bad, etc etc, nod nod.
      zle_highlight+=(paste:none)

      zsh-newuser-install() { :; }
    '';
    loginShellInit = ''
      if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] ;
        then exec startx;
      fi
    '';
    promptInit = "";
  };

  users.users.cjb.shell = pkgs.zsh;

  # system wide
  environment.variables = {
    EDITOR = "ed";
    VISUAL = "ed";
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
      url = https://github.com/nix-community/emacs-overlay/archive/458d30ef17167e390d0280d0f954ca8ee61ef701.tar.gz;
    }))
  ];

  system.stateVersion = "20.09";
}
