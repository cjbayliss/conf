{ config, pkgs, lib, ... }:

with pkgs;
let
  chromium = (ungoogled-chromium.override {
    commandLineArgs = ''
      $([ $(date "+%k") -ge 17 ] || [ $(date "+%k") -le 5 ] && echo "--force-dark-mode --enable-features=WebUIDarkMode")'';
  });
  mpv = (mpv-with-scripts.override { scripts = [ mpvScripts.mpris ]; });
  python = python3.withPackages (pp: with pp; [ flake8 notify2 pylint ]);
in {
  # the bad
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "b43-firmware" ];

  imports = [
    # hardware
    ./hardware-configuration.nix
    # display server config
    ./display-server.nix
    # hardening config
    ./harden.nix
  ];

  boot.loader = {
    efi = { canTouchEfiVariables = false; };
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
    hsetroot
    libnotify
    opusTools
    pciutils
    pulseaudio # for pactl
    starship
    universal-ctags
    xmobar

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
    nixfmt
    proselint
    shellcheck

    # tools
    beets
    efibootmgr
    feh
    ffmpeg
    git
    glirc
    imagemagick
    ix
    neomutt
    pandoc
    pass
    playerctl
    tmux
    neovim
    unzip
    w3m
    wget
    youtube-dl

    # gui
    chromium
    dmenu
    firefox
    krita
    mpv
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
  ];

  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  programs.light.enable = true;
  programs.ssh.startAgent = true;

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

  virtualisation.podman.enable = true;

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
      baekmuk-ttf
      iosevka-bin
      ipafont
      liberation_ttf
      noto-fonts-emoji
    ];
    fontconfig = {
      defaultFonts.emoji = [ "Noto Color Emoji" ];
      defaultFonts.monospace =
        [ "Iosevka Fixed" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      defaultFonts.sansSerif =
        [ "Liberation Sans" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
      defaultFonts.serif =
        [ "Liberation Serif" "IPAGothic" "Baekmuk Gulim" "Noto Color Emoji" ];
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
    shellInit = ''
      set fish_greeting

      # allow urls with ? in them
      set -U fish_features qmark-noglob

      # colors
      set -U fish_color_autosuggestion      brblack
      set -U fish_color_cancel              -r
      set -U fish_color_command             'white' '--bold'
      set -U fish_color_comment             green
      set -U fish_color_cwd                 green
      set -U fish_color_cwd_root            red
      set -U fish_color_end                 brmagenta
      set -U fish_color_error               brred
      set -U fish_color_escape              brcyan
      set -U fish_color_history_current     --bold
      set -U fish_color_host                normal
      set -U fish_color_match               --background=brblue
      set -U fish_color_normal              normal
      set -U fish_color_operator            normal
      set -U fish_color_param               normal
      set -U fish_color_quote               yellow
      set -U fish_color_redirection         bryellow
      set -U fish_color_search_match        'bryellow' '--background=brblack'
      set -U fish_color_selection           'white' '--bold' '--background=brblack'
      set -U fish_color_status              red
      set -U fish_color_user                brgreen
      set -U fish_color_valid_path          --underline
      set -U fish_pager_color_completion    normal
      set -U fish_pager_color_description   yellow
      set -U fish_pager_color_prefix        'white' '--bold' '--underline'
      set -U fish_pager_color_progress      '-r' 'brblack'

      # prompt
      starship init fish | source
    '';
    loginShellInit = ''
      if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]
          exec startx
      end
    '';
  };

  users.users.cjb.shell = pkgs.fish;

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

    EDITOR = "nvim";
    VISUAL = "nvim";

    # FIXME: make this *only* get set for 'cjb'
    EMAIL = "cjb@cjb.sh";
    NAME = "Christopher Bayliss";
  };

  system.stateVersion = "20.09";
}
