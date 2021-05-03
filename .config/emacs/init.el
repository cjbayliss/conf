;; Various functions and configurations for GNU Emacs
;;
;; Written in 2018, 2019, 2020, 2021 by Christopher Bayliss <cjb@cjb.sh>
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
 browse-url-browser-function 'browse-url-firefox
 c-basic-offset 4
 column-number-mode t
 custom-file (concat user-emacs-directory "/custom.el")
 dired-listing-switches "-ABlhF"
 doom-modeline-height 20
 doom-modeline-icon nil
 eshell-ls-initial-args "-h"
 ido-enable-flex-matching t
 ido-ignore-buffers '("\\` " "^\*")
 inferior-lisp-program "sbcl --no-userinit"
 inhibit-startup-screen t
 initial-scratch-message nil
 make-backup-files nil
 mouse-yank-at-point t
 package--init-file-ensured t
 package-enable-at-startup nil
 require-final-newline t
 scheme-program-name "csi -n"
 webpaste-paste-confirmation t
 webpaste-provider-priority '("dpaste.org" "bpa.st")

 ;; eww
 eww-download-directory (expand-file-name "~/downloads")
 eww-header-line-format nil
 eww-search-prefix "https://duckduckgo.com/lite/?q="
 shr-cookie-policy nil
 shr-discard-aria-hidden t
 shr-max-image-proportion 0.6
 shr-use-colors nil
 shr-use-fonts nil

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
(global-set-key (kbd "C-c C-b") 'vc-msg-show)
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c e") 'ido-emoji)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c l") 'run-lisp)
(global-set-key (kbd "C-c m") 'proced)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c p") 'run-python)
(global-set-key (kbd "C-c s") 'run-scheme)
(global-set-key (kbd "C-c v") 'cterm)
(global-set-key (kbd "C-c w b") 'webpaste-paste-buffer)
(global-set-key (kbd "C-c w r") 'webpaste-paste-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; see https://github.com/jorgenschaefer/circe/wiki/Configuration
;; this function is probably under the GPL3, at least that is what circe
;; is licensed as.
;; begin probable GPL3 code
(defun my/fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun my/return-password (server)
  (my/fetch-password :host server))
;; end probable GPL3 code

;; custom irc function
(defun irc ()
  "Connect to IRC."
  (interactive)

  (require 'circe)
  (require 'circe-color-nicks)
  ;; better nick colours
  (require 'erc-hl-nicks)
  (setf (symbol-function 'circe-nick-color-for-nick)
        (symbol-function 'erc-hl-nicks-color-for-nick))
  (enable-circe-color-nicks)

  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix "      "))

  (setq lui-fill-type nil
        lui-time-stamp-position 'left
        lui-time-stamp-format "%H:%M "
        lui-time-stamp-only-when-changed-p nil
        circe-default-part-message ""
        circe-default-quit-message ""
        circe-fool-list '("{^_^}" "gnUser")
        circe-format-say "<{nick}> {body}"
        circe-format-action "[{nick} {body}]"
        circe-format-self-say circe-format-say
        circe-format-self-action circe-format-action
        circe-reduce-lurker-spam t
        circe-color-nicks-everywhere t
        lui-flyspell-p t
        circe-default-nick "cjbayliss"
        circe-default-realname "Christopher Bayliss"
        circe-network-options
        '(("OFTC"
           :tls t
           :host "irc.oftc.net"
           :nick "cjbayliss"
           :nickserv-password my/return-password
           :channels (:after-auth "#llvm"))
          ("Cyber"
           :host "127.0.0.1"
           :port "6667"
           :nick "cjb"
           :channels ("#cyber"))
          ("Freenode"
           :tls t
           :host "chat.au.freenode.net"
           :nick "cjb"
           :sasl-strict t
           :sasl-username "cjb"
           :sasl-password my/return-password
           :channels (:after-auth "#chicken"
                                  "##lisp"
                                  "#python"
                                  "##rust"
                                  "#scheme"))))
  (circe "OFTC")
  (circe "Freenode")

  (custom-set-faces
   '(circe-my-message-face ((t (:foreground "gray60" :slant oblique))))
   '(circe-originator-face ((t (:foreground "#f78fe7")))))

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what
  ;; life it has lived, but since you know how to use git you'll find
  ;; out in no time!! (yes, I felt like writing about this paren for no
  ;; reason at all.)
  )

;; separate cyber irc function
(defun irc-cyber ()
  (interactive)
  (if (featurep 'circe)
      (circe "Cyber")
    (error "circe not setup, try M-x irc RET first")))

;; load php stuff grumble grumble
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (require 'php-mode)
                               (php-mode)
                               (setq c-basic-offset 4
                                     indent-tabs-mode nil)
                               (php-enable-psr2-coding-style))))

;; rust
(add-to-list 'auto-mode-alist
             '("\\.rs\\|.rlib\\'" . (lambda ()
                                      (require 'rust-mode)
                                      (rust-mode)
                                      (setq rust-format-on-save t))))

;; nix
(add-to-list 'auto-mode-alist
             '("\\.nix\\'" . (lambda ()
                               (require 'nix-mode)
                               (nix-mode))))

;; if I'm editing a C file, I *probably* want the linux style
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux"
                  c-basic-offset 8
                  tab-width 8
                  indent-tabs-mode t)))

;; passing --eval in inferior-lisp-program is broken
(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (lisp-eval-string "(require 'sb-aclrepl)")))

;; miscellaneous dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            ;; first up, don't create lots of dired buffers
            (put 'dired-find-alternate-file 'disabled nil)
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
            ;; also, quit means quit, please!
            (define-key dired-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))
            ;; nice colouring in dired
            (require 'diredfl)
            (diredfl-mode)))

;; https://stackoverflow.com/a/47587185
;; begin unknown licensed code
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*"
                   (cons #'display-buffer-no-window nil)))
;; end unknown licensed code

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

;; programming mode settings
(add-hook 'prog-mode-hook
          (lambda ()
            (require 'highlight-numbers)
            (highlight-numbers-mode +1)
            (hl-line-mode +1)
            (display-line-numbers-mode +1)
            (setq show-trailing-whitespace t)))

;; can global-hl-mode-mode be disabled *just* for IRC?
(mapc (lambda (x)
        (add-hook x 'hl-line-mode +1))
      '(dired-mode-hook
        text-mode-hook))

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

;; default startup message
(defun display-startup-echo-area-message ()
  (message (concat "In the beginning the Emacs was created. This has "
                   "made a lot of people very angry and been widely "
                   "regarded as a bad move.")))

;; list of emoji I'm likely to use
(defvar ido-emoji-list
  '("🙂" "🤷" "🤦" "🥳" "🤣" "🤨" "😜" "😬" "👋" "👍" "👌" "😱"
    "😭" "😑" "💃"))

(defun build-ido-emoji-list ()
  "Return a list of emoji with their Unicode names built from the
`ido-emoji-list'."
  (let (emoji-list)
    (dolist (emoji ido-emoji-list)
      (push (format "%s %s"
                    emoji
                    (get-char-code-property (string-to-char emoji)
                                            'name))
            emoji-list))
    (nreverse emoji-list)))

(defun ido-emoji ()
  "An emoji picker!"
  (interactive)
  (insert
   (substring
    (ido-completing-read "Insert emoji: " (build-ido-emoji-list)) 0 1)))

;; eshell stuff
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "cat")
            (require 'fish-completion)
            (global-fish-completion-mode)))

;; please let me cut and paste, and other normal things
(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode +1)
            (setq comint-process-echoes t)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            ;; quoted paste
            (define-key term-raw-map (kbd "C-c C-y")
              (lambda ()
                (interactive)
                (term-send-raw-string (format "\"%s\"" (current-kill 0)))))
            (define-key term-raw-map (kbd "C-k")
              (lambda ()
                (interactive)
                (term-send-raw-string "\C-k")
                (kill-line)))))

;; always kill-buffer after exit
(advice-add 'term-handle-exit :filter-return #'kill-buffer)

;; cterm, my first initial + term, yeah, so creative right?!! 🤦
(defun cterm ()
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/run/current-system/sw/bin/fish")))

;; make sure these directories exists
(unless (file-directory-p (concat user-emacs-directory "lisp"))
  (make-directory (concat user-emacs-directory "lisp") t))
(unless (file-directory-p (concat user-emacs-directory "emms"))
  (make-directory (concat user-emacs-directory "emms") t))

;; add site lisp
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
      '(circe
        diredfl
        elpher
        emms
        erc-hl-nicks
        fish-completion
        highlight-numbers
        modus-themes
        nix-mode
        php-mode
        rust-mode
        vc-msg
        webpaste
        which-key))

;; autoloads
(mapc (lambda (x)
        (autoload x (symbol-name x) nil t))
      '(elpher emms-browser))
(autoload 'vc-msg-show "vc-msg" nil t)
(autoload 'webpaste-paste-buffer "webpaste" nil t)
(autoload 'webpaste-paste-region "webpaste" nil t)

;; NOTE: everything after here should go last.

;; GUI config
(when (display-graphic-p)
  (require 'modus-themes)
  (setq modus-themes-slanted-constructs t
        modus-themes-no-mixed-fonts t)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  (custom-set-faces '(bold ((t (:weight semi-bold)))))

  ;; !@#$%^ FONTS
  (when (member "Iosevka Fixed" (font-family-list))
    (set-frame-font "Iosevka Fixed-11" 'keep-size t))
  ;; 😜
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))
  ;; 한국어/조선말
  (when (member "Baekmuk Gulim" (font-family-list))
    (set-fontset-font t 'unicode "Baekmuk Gulim-11" nil 'prepend))
  ;; 日本語
  (when (member "IPAGothic" (font-family-list))
    (set-fontset-font t 'unicode "IPAGothic-11" nil 'prepend))
  ;; Latin/Cyrillic
  (when (member "Iosevka Fixed" (font-family-list))
    (set-fontset-font t 'unicode "Iosevka Fixed-11" nil 'prepend))

  (setq x-gtk-use-system-tooltips nil)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (server-start))

;; put slow modes &c in this hook for a faster startup! 🥳
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; setup the modeline (in this hook to only get run once)
            (delete (nth 4 mode-line-modes) mode-line-modes)
            (setq-default mode-line-format
                          '("%e"
                            mode-line-front-space
                            mode-line-mule-info
                            mode-line-client
                            (:eval (if (buffer-modified-p)
                                       (format-mode-line
                                        'mode-line-modified 'warning)
                                     mode-line-modified))
                            mode-line-remote
                            mode-line-frame-identification
                            mode-line-buffer-identification
                            "   "
                            mode-line-position
                            (vc-mode vc-mode)
                            "  "
                            (:eval (when (boundp 'tracking-max-mode-line-entries)
                                     tracking-mode-line-buffers))
                            (:eval (format-mode-line 'mode-line-modes
                                                     'font-lock-doc-face))
                            (:eval (format-mode-line
                                    '(" " display-time-string) 'bold))
                            "  "
                            (:eval (format-mode-line
                                    mode-line-misc-info
                                    'font-lock-comment-delimiter-face))
                            mode-line-end-spaces))

            ;; enable/disable modes
            (delete-selection-mode +1)
            (display-time-mode +1)
            (delq 'display-time-string global-mode-string)
            (savehist-mode +1)
            (show-paren-mode +1)
            (require 'which-key)
            (which-key-mode)

            ;; restore default gc-cons-*
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))
