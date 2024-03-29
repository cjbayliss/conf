{ config, pkgs, ... }:

with pkgs;
let
  chromium = (ungoogled-chromium.override {
    commandLineArgs = "--force-dark-mode --enable-features=WebUIDarkMode --no-referrers --js-flags=--noexpose_wasm --no-pings ";
  });
  mpv = (mpv-with-scripts.override { scripts = [ mpvScripts.mpris ]; });
  python = python3.withPackages (pp: with pp; [ flake8 notify2 pylint ]);
in
{
  imports = [
    # hardware
    ./hardware-configuration.nix
    ./machine/default.nix
    # display server config
    ./display-server.nix
  ];

  nix.autoOptimiseStore = true;

  boot.supportedFilesystems = [ "zfs" ];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  services.resolved = {
    enable = true;
    dnssec = "true";
    extraConfig = ''
      DNS=9.9.9.9
      DNSOverTLS=yes
    '';
    fallbackDns = [ "149.112.112.112" ];
  };

  environment.etc."issue".enable = false;

  time.timeZone = "Australia/Melbourne";

  i18n.defaultLocale = "en_AU.utf8";
  console = {
    font = "";
    keyMap = "us";
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  users.users.cjb = {
    isNormalUser = true;
    extraGroups = [
      "audio"
      "video"
      "wheel"
    ];
    shell = pkgs.fish;
    description = "Christopher Bayliss";
    packages = with pkgs; [ ];
  };

  # auto login
  services.getty.autologinUser = "cjb";

  environment.systemPackages = with pkgs; [
    # libs
    dmenu
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
    lua
    php74
    python
    sbcl

    # langs-extras
    black
    fnlfmt
    hlint
    luaformatter
    nix-linter
    nixpkgs-fmt
    proselint
    shellcheck

    # tools
    appimage-run
    beets
    efibootmgr
    feh
    ffmpeg
    git
    htop
    logiops
    neovim
    pandoc
    pass
    playerctl
    scrot
    tmux
    unzip
    w3m
    wget
    yt-dlp

    # gui
    alacritty
    chromium
    emacs
    j4-dmenu-desktop
    mpv
  ];

  # for Logitech M720 mouse
  systemd.services.logid = {
    enable = true;
    description = "Logitech Configuration Daemon";

    unitConfig = {
      After = [ "multi-user.target" ];
      Wants = [ "multi-user.target" ];
    };

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.logiops}/bin/logid";
      ExecReload = "/bin/kill -HUP $MAINPID";
      Restart = "on-failure";

      # lockdown the service
      CapabilityBoundingSet = "";
      IPAddressDeny = "any";
      LockPersonality = "yes";
      MemoryDenyWriteExecute = "yes";
      NoNewPrivileges = "yes";
      PrivateMounts = "yes";
      PrivateNetwork = "yes";
      PrivateTmp = "yes";
      PrivateUsers = "yes";
      ProtectControlGroups = "yes";
      ProtectHome = "yes";
      ProtectKernelModules = "yes";
      ProtectKernelTunables = "yes";
      ProtectSystem = "strict";
      RestrictRealtime = "yes";
      UMask = "0077";
    };

    wantedBy = [ "graphical.target" ];
  };

  # flatpak
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ xdg-desktop-portal-gtk ];

  sound.enable = true;
  security.rtkit.enable = true;
  hardware.pulseaudio.enable = false;
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
    pinentryFlavor = "qt";
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

  programs.fish = {
    enable = true;
    shellInit = ''
        set fish_greeting

        # allow urls with '?' in them
        set -U fish_features qmark-noglob

        # colors
        set -U fish_color_autosuggestion      brblue
        set -U fish_color_cancel              -r
        set -U fish_color_command             'white' '--bold'
        set -U fish_color_comment             brblue
        set -U fish_color_cwd                 brcyan
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
        set -U fish_color_user                green
        set -U fish_color_valid_path          --underline
        set -U fish_pager_color_completion    normal
        set -U fish_pager_color_description   yellow
        set -U fish_pager_color_prefix        'white' '--bold' '--underline'
        set -U fish_pager_color_progress      '-r' 'white'

        # prompt
        function __git_branch
            git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/'
        end

        function __git_status
            if [ (git ls-files 2>/dev/null | wc -l) -lt 2000 ]
                git status --short 2>/dev/null | sed 's/^ //g' | cut -d' ' -f1 | sort -u | tr -d '\n' | sed 's/^/ /'
            else
                printf " [NOSTAT]"
            end
        end

        function fish_right_prompt
            set -l last_status $status
            if [ $last_status -ne 0 ]
                set_color --bold $fish_color_error
                printf '%s ' $last_status
                set_color normal
            end
        end

      function fish_prompt
          # host
          set_color normal
          printf '%s ' (prompt_hostname)

          # pwd
          set_color $fish_color_cwd
          echo -n (prompt_pwd)
          set_color normal

          # git stuff
          set_color brmagenta
          printf '%s' (__git_branch)
          set_color magenta
          printf '%s ' (__git_status)
          set_color normal

          # prompt delimiter
          echo -n '» '
      end
    '';

    loginShellInit = ''
      if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]
          exec sx
      end
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

    EDITOR = "nvim";
    VISUAL = "nvim";

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

}
