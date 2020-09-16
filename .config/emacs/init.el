;; Various functions and configurations for GNU Emacs
;;
;; Written in 2018, 2019, 2020 by Christopher Bayliss <cjb@cjb.sh>
;;
;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to the
;; public domain worldwide. This software is distributed without any
;; warranty.
;;
;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software. If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;; general emacs settings
(setq
 browse-url-browser-function 'w3m-browse-url
 c-basic-offset 4
 column-number-mode t
 custom-file (concat user-emacs-directory "/custom.el")
 dired-listing-switches "-alhF"
 inhibit-startup-screen t
 initial-scratch-message nil
 make-backup-files nil
 mouse-yank-at-point t
 package--init-file-ensured t
 package-enable-at-startup nil
 require-final-newline t
 scheme-program-name "csi -n"

 ;; w3m
 w3m-add-user-agent nil
 w3m-bookmark-file-coding-system 'utf-8
 w3m-coding-system 'utf-8
 w3m-default-coding-system 'utf-8
 w3m-default-display-inline-images t
 w3m-default-save-directory "~/downloads/"
 w3m-display-mode 'tabbed
 w3m-file-coding-system 'utf-8
 w3m-file-name-coding-system 'utf-8
 w3m-fill-column 120
 w3m-home-page "about:blank"
 w3m-key-binding 'info
 w3m-profile-directory (concat (getenv "XDG_CACHE_HOME") "/w3m")
 w3m-search-default-engine "duckduckgo"
 w3m-search-engine-alist
 '(("wikipedia" "https://en.wikipedia.org/wiki/Special:Search/%s")
   ("duckduckgo" "https://lite.duckduckgo.com/lite" nil "q=%s"))
 w3m-terminal-coding-system 'utf-8
 w3m-track-mouse nil
 w3m-use-cookies nil
 w3m-use-favicon nil
 w3m-use-symbol t
 w3m-use-toolbar nil

 ;; for faster startup
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6

 ;; get stuff out of the home dir
 gnus-directory (concat user-emacs-directory "news")
 gnus-startup-file (concat user-emacs-directory "newsrc")
 gnus-init-file (concat user-emacs-directory "gnus"))

(setq-default
 fill-column 72                         ; 72 is better
 frame-background-mode 'dark
 indent-tabs-mode nil
 show-trailing-whitespace nil)

;; enable/disable modes that can't go in the startup hook
(menu-bar-mode -1)
(save-place-mode +1)
(ido-mode 'buffers)                     ; only for buffer switching

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-ch" 'hl-line-mode)
(global-set-key "\C-cl" 'display-line-numbers-mode)
(global-set-key "\C-cs" 'run-scheme)

;; FIXME: switch to SASL. I tried circe, but almost pulled my hair out.
;; custom irc func to load erc and join networks automatically
(defun irc ()
  "Connect to IRC."
  (interactive)

  ;; these bits need to be here **before** you start ERC
  (setq erc-prompt-for-nickserv-password nil
        erc-fill-column 157)            ; the auto resize is below

  (load (concat user-emacs-directory "erc"))
  (require 'erc-services)
  (erc-services-mode +1)

  (erc-tls :server "chat.au.freenode.net"
           :port 6697
           :nick "cjb"
           :full-name "Christopher Bayliss")
  (erc-tls :server "irc.oftc.net"
           :port 6697
           :nick "cjbayliss"
           :full-name "Christopher Bayliss"))

;; ERC config
(with-eval-after-load "erc"
  (require 'erc-goodies)

  (setq erc-autojoin-channels-alist
        '(("freenode.net"
           "#chicken"
           "#gentoo-hardened"
           "#gentoo-lisp"
           "##lisp"
           "#scheme"
           "#xebian"))
        erc-autojoin-timing 'ident
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-prompt-for-nickserv-password nil
        erc-prompt-for-password nil
        erc-rename-buffers t
        erc-server "chat.au.freenode.net"
        erc-server-reconnect-timeout 300
        erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-user-full-name "Christopher Bayliss")

  (erc-scrolltobottom-enable)
  (erc-spelling-mode +1)
  (require 'erc-hl-nicks)
  (erc-hl-nicks)

  ;; make ERC use full buffer width
  (add-to-list 'window-configuration-change-hook
               (lambda () (setq erc-fill-column (- (window-width) 2))))
  ;; keep ERC buffer pined to bottom
  (add-to-list 'erc-mode-hook
               (lambda ()
                 (set
                  (make-local-variable 'scroll-conservatively) 100)))

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what
  ;; life it has lived, but since you know how to use git you'll find
  ;; out in no time!! (yes, I felt like writing about this paren for no
  ;; reason at all.)
  )

;; load php stuff grumble grumble
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (require 'php-mode)
                               (php-mode)
                               (setq c-basic-offset 4
                                     indent-tabs-mode nil)
                               (php-enable-psr2-coding-style))))

;; load nasm-mode instead of the broken(?) asm-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(asm\\|nasm\\|S\\|s\\)\\'" . (lambda ()
                                                    (require 'nasm-mode)
                                                    (nasm-mode))))

;; if I'm editing a C file, I *probably* want the linux style
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux"
                  c-basic-offset 8
                  tab-width 8
                  indent-tabs-mode t)))

;; instead of loading hl-todo
(defface highlight-todo-face
  '((t :inherit font-lock-warning-face
       :weight bold
       :slant italic))
  "Basic face for highlighting TODO &c.")
(defvar highlight-todo-face 'highlight-todo-face)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):"
                1 highlight-todo-face t)))))

;; emacs should already have this function, does it and i can't find it?
(defun byte-compile-directory (directory)
  "compile contents of a directory"
  (interactive "DByte compile directory: ")
  (byte-recompile-directory directory 0))

;; play/pause music, or start playing at random if nothing is playing
(defun emms-play/pause-handler ()
  "determine best course of action when pressing play/pause button"
  (interactive)
  (unless (featurep 'emms)
    (emms-browser))
  (defun emms-random-play-all ()
    "hacky solution to play all songs in random mode."
    (emms-browse-by-performer)
    (emms-browser-add-tracks)
    (emms-shuffle)
    (emms-start))
  (if (or (not emms-player-playing-p)
          emms-player-stopped-p)
      (emms-random-play-all)
    (emms-pause)))

;; alsa volume control
(defun alsa-raise-volume ()
  (interactive)
  (shell-command
   "amixer set Master 2%+ unmute | grep 'Mono:' | cut -d' ' -f6"))

(defun alsa-lower-volume ()
  (interactive)
  (shell-command
   "amixer set Master 2%- unmute | grep 'Mono:' | cut -d' ' -f6"))

(defun alsa-mute ()
  (interactive)
  (shell-command
   "amixer set Master toggle | grep 'Mono:' | cut -d' ' -f8"))

;; back-light control
(defun back-light-up ()
  (interactive)
  (shell-command
   "brightnessctl set +5% | grep Current | cut -d' ' -f4"))

(defun back-light-down ()
  (interactive)
  (shell-command
   "brightnessctl set 5%- | grep Current | cut -d' ' -f4"))

;; programming mode settings
(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode +1)
            (display-line-numbers-mode +1)
            (setq show-trailing-whitespace t)))

;; can global-hl-mode-mode be disabled *just* for ERC?
(mapc (lambda (x)
        (add-hook x 'hl-line-mode +1))
      '(text-mode-hook
        dired-mode-hook
        w3m-mode-hook))

;; emms config
(add-hook 'emms-browser-mode-hook
          (lambda ()
            (require 'emms-setup)
            (require 'emms-info)
            (emms-standard)
            (emms-all)
            (emms-default-players)
            (setq emms-player-list (list emms-player-mpv)
                  emms-mode-line-format " [ %s"
                  emms-playing-time-display-format " | %s ] "
                  emms-source-file-default-directory "~/music/"
                  emms-mode-line-mode-line-function
                  'emms-mode-line-playlist-current)
            (add-to-list 'emms-player-base-format-list "opus")
            (emms-player-set emms-player-mpv 'regex
                             (apply #'emms-player-simple-regexp
                                    emms-player-base-format-list))))

;; make sure these directories exists
(unless (file-directory-p (concat user-emacs-directory "lisp"))
  (make-directory (concat user-emacs-directory "lisp") t))
(unless (file-directory-p (concat user-emacs-directory "emms"))
  (make-directory (concat user-emacs-directory "emms") t))
(unless (file-directory-p (concat (getenv "XDG_CACHE_HOME") "/w3m"))
  (make-directory (concat (getenv "XDG_CACHE_HOME") "/w3m") t))

;; add site lisp
(let ((default-directory  "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat user-emacs-directory "lisp")))
  (normal-top-level-add-subdirs-to-load-path))

;; ensure packages
(mapc (lambda (x)
        (unless (locate-library (symbol-name x))
          (unless (featurep 'package)
            (require 'package)
            (add-to-list 'package-archives
                         '("melpa" . "https://melpa.org/packages/") t)
            (setq package-user-dir (concat user-emacs-directory "lisp"))
            (package-initialize)
            (package-refresh-contents))
          (package-install x)))
      '(elpher
        emms
        erc-hl-nicks
        exwm
        modus-operandi-theme
        modus-vivendi-theme
        nasm-mode
        php-mode
        vterm
        w3m
        which-key))

;; autoloads
(mapc (lambda (x)
        (autoload x (symbol-name x) nil t))
      '(elpher emms-browser vterm w3m))
(autoload 'w3m-browse-url "w3m" nil t)

;; NOTE: everything after here should go last.

(mapc (lambda (x)
        (add-to-list
         'custom-theme-load-path
         (car (file-expand-wildcards
               (concat user-emacs-directory "lisp/"
                       (symbol-name x) "*")))))
      '(modus-operandi modus-vivendi))

(defun dark-background-mode ()
  "set the background mode to dark"
  (disable-theme 'modus-operandi)
  (setq-default frame-background-mode 'dark)
  (set-background-color "black")
  (set-foreground-color "white")
  (load-theme 'modus-vivendi t))

(defun light-background-mode ()
  "set the background mode to light"
  (disable-theme 'modus-vivendi)
  (setq-default frame-background-mode 'light)
  (set-background-color "white")
  (set-foreground-color "black")
  (load-theme 'modus-operandi t))

(defun toggle-background-mode ()
  "toggle between light and dark mode"
  (interactive)
  (if (eq frame-background-mode 'light)
      (dark-background-mode)
    (light-background-mode))
  (when (featurep 'erc-hl-nicks)
    (erc-hl-nicks-refresh-colors)))

(defun start-exwm ()
  "start exwm"
  (require 'exwm)
  (require 'exwm-xim)

  (setq exwm-input-global-keys
        `(([f6] . toggle-background-mode)
          ([XF86AudioLowerVolume] . alsa-lower-volume)
          ([XF86AudioRaiseVolume] . alsa-raise-volume)
          ([XF86AudioMute] . alsa-mute)
          ([XF86MonBrightnessDown] . back-light-down)
          ([XF86MonBrightnessUp] . back-light-up)
          ([XF86AudioNext] . emms-next)
          ([XF86AudioPlay] . emms-play/pause-handler)
          ([XF86AudioPrev] . emms-previous)
          ([XF86LaunchB] . (lambda (command)
                             (interactive (list
                                           (read-shell-command "Run: ")))
                             (start-process-shell-command command nil
                                                          command))))

        exwm-input-simulation-keys
        '(([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (exwm-xim-enable)
  (push ?\C-\\ exwm-input-prefix-keys)
  (exwm-enable))

;; GUI config
(when (display-graphic-p)
  ;; https://www.youtube.com/watch?v=UbxUSsFXYo4
  ;; workin' 9 to 5, what a way to make a livin'
  (if (and (>= (string-to-number (format-time-string "%H%M")) 0900)
           (<= (string-to-number (format-time-string "%H%M")) 1700))
      (light-background-mode)
    (dark-background-mode)))

;; put slow modes &c in this hook for a faster startup! 🥳
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; restore default gc-cons-*
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
            (custom-set-faces
             '(bold ((t (:weight semi-bold)))))
            ;; enable/disable modes
            (delete-selection-mode +1)
            (display-time-mode +1)
            (show-paren-mode +1)
            (require 'which-key)
            (which-key-mode)
            (when (display-graphic-p)
              (when (member "Iosevka Term" (font-family-list))
                (set-frame-font "Iosevka Term Medium-11" t t))
              (when (member "Noto Color Emoji" (font-family-list))
                (set-fontset-font t 'unicode
                                  "Noto Color Emoji" nil 'prepend))
              (setq-default cursor-type '(hbar . 2))
              (fringe-mode 0)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)
              (start-exwm))))
