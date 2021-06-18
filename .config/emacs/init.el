;;; Init --- GNU Emacs initialisation file  -*- lexical-binding: t -*-

;; Author: Christopher Bayliss <cjb@cjb.sh>
;; Created: 2021-05-05
;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Complete rewrite of my old Emacs initialisation file

;; Raison d'être: readability > correctness > speed

;;; Code:
;; begin init
(setq gc-cons-threshold most-positive-fixnum)

;;; General:
;;;; sane defaults
(setq auth-source-save-behavior nil)
(setq browse-url-handlers '(("youtube" . browse-url-mpv)
                            ("youtu.be" . browse-url-mpv)
                            ("." . browse-url-firefox)))
(setq c-basic-offset 4)
(setq column-number-mode t)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq help-window-select t)
(setq kill-read-only-ok t)
(setq make-backup-files nil)
(setq org-cycle-include-plain-lists 'integrate)
(setq package--init-file-ensured t)
(setq require-final-newline t)
(setq shell-file-name "sh")
(setq-default fill-column 72)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)

;;;; misc options/changes
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)

;; don't popup the async-shell-command buffer
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*"
                   (cons #'display-buffer-no-window nil)))

;;;; modus themes
(setq modus-themes-slanted-constructs t)
(setq modus-themes-no-mixed-fonts t)
(load-theme 'modus-vivendi)

;;;; disable/enable various global modes
(menu-bar-mode -1)
(save-place-mode +1)

;; these modes are slow to load, add them to this hook instead
(add-hook 'emacs-startup-hook
          (lambda ()
            (delete-selection-mode +1)
            (savehist-mode +1)
            (show-paren-mode +1)))

;; global-hl-line-mode is overkill
(mapc (lambda (x)
        (add-hook x 'hl-line-mode +1))
      '(dired-mode-hook
        text-mode-hook))

;;;; disable/change startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message (concat "In the beginning the Emacs was created. This has "
                   "made a lot of people very angry and been widely "
                   "regarded as a bad move.")))

;;;; setup fonts
(when (display-graphic-p)
  ;; default font
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
    (set-fontset-font t 'unicode "Iosevka Fixed-11" nil 'prepend)))

;;;; setup GUI only stuff
(when (display-graphic-p)
  (setq mouse-yank-at-point t)
  (setq x-gtk-use-system-tooltips nil)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (server-start))

;;;; keybindings
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c m") 'proced)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c p") 'run-python)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;; configuring the mode-line is pretty ugly 🤮
(add-hook
 'emacs-startup-hook
 (lambda ()
   (delete (nth 4 mode-line-modes) mode-line-modes)
   (setq-default
    mode-line-format
    '("%e"
      mode-line-front-space
      mode-line-mule-info
      mode-line-client
      (:eval (if (buffer-modified-p)
                 (format-mode-line 'mode-line-modified 'warning)
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
      (:eval (format-mode-line 'mode-line-modes 'font-lock-doc-face))
      (:eval (format-mode-line '(" " display-time-string) 'bold))
      "  "
      (:eval (format-mode-line mode-line-misc-info
                               'font-lock-comment-delimiter-face))
      mode-line-end-spaces))

   (display-time-mode +1)
   (delq 'display-time-string global-mode-string)))

;;; Tools
;;;; Circe
;;;;; Circe options
(with-eval-after-load 'circe
  (setq lui-fill-type nil)
  (setq lui-logging-directory "~/stuff/irc-logs")
  (setq lui-time-stamp-format "%H:%M ")
  (setq lui-time-stamp-only-when-changed-p nil)
  (setq lui-time-stamp-position 'left)

  (setq circe-default-part-message "")
  (setq circe-default-quit-message "")
  (setq circe-fool-list '("{^-^}"
                          "Hash"
                          "epony"
                          "gnUser"))
  (setq circe-format-say "<{nick}> {body}")
  (setq circe-format-action "[{nick} {body}]")
  (setq circe-format-self-say circe-format-say)
  (setq circe-format-self-action circe-format-action)
  (setq circe-reduce-lurker-spam t)
  (setq circe-color-nicks-everywhere t)
  (setq lui-flyspell-p t)
  (setq circe-default-nick "cjbayliss")
  (setq circe-default-realname "Christopher Bayliss")

  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix "      "))
  (require 'lui-logging)
  (enable-lui-logging-globally))

;;;;; nick colours etc
(with-eval-after-load 'circe
  (require 'circe-color-nicks)
  (setf (symbol-function 'circe-nick-color-for-nick)
        (symbol-function 'irc-nick-color))
  (enable-circe-color-nicks)

  (set-face-attribute 'circe-my-message-face nil :inherit font-lock-comment-face)
  (set-face-attribute 'circe-originator-face nil :inherit font-lock-preprocessor-face))

;;;;; circe network options
(with-eval-after-load 'circe
  (setq
   circe-network-options
   '(("OFTC"
      :tls t
      :host "irc.oftc.net"
      :nick "cjbayliss"
      :nickserv-password (lambda (x)
                           (auth-source-pass-get 'secret "irc.oftc.net"))
      :channels (:after-auth "#llvm"))
     ("Cyber"
      :host "127.0.0.1"
      :port "6667"
      :nick "cjb"
      :channels ("#cyber"))
     ("Libera.Chat"
      :tls t
      :port 6697
      :host "irc.libera.chat"
      :nick "cjb"
      :sasl-strict t
      :sasl-username "cjb"
      :sasl-password (lambda (x)
                       (auth-source-pass-get 'secret "irc.libera.chat"))
      :channels (:after-auth "#chicken"
                             "#emacs"
                             "#lisp"
                             "#commonlisp"
                             "#python"
                             "##rust"
                             "#scheme"
                             "#xebian")))))

;;;;; circe functions
(defun irc ()
  "Connect to IRC."
  (interactive)
  (require 'circe)
  (circe "OFTC")
  (circe "Libera.Chat"))

(defun irc-cyber ()
  (interactive)
  (if (featurep 'circe)
      (circe "Cyber")
    (error "circe not setup, try M-x irc RET first")))

;;;; dired
(setq dired-listing-switches "-ABlhFv")

(add-hook 'dired-mode-hook
          (lambda ()
            ;; first up, don't create lots of dired buffers
            (put 'dired-find-alternate-file 'disabled nil)
            (define-key
              dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
            ;; also, quit means quit, please!
            (define-key dired-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))))

;;;; eshell
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 4096)
(setq eshell-input-filter 'eshell-input-filter-initial-space)
(setq eshell-ls-initial-args "-h")
(setq eshell-scroll-to-bottom-on-input 'all)

;; FIXME: test if still broken after next rebuild of emacs!!
;;
;; eshell-watch-for-password-prompt appears broken, maybe because I'm on
;; NixOS? or maybe because I built emacs from master? Either way, eshell
;; *and* tramp fail with eshell-watch-for-password-prompt
(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'em-unix)
            (delq 'eshell-watch-for-password-prompt
                  eshell-output-filter-functions)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (goto-address-mode +1)
            (setenv "PAGER" "cat")
            ;; these in term
            (add-to-list 'eshell-visual-commands "mpv")
            (add-to-list 'eshell-visual-commands "nix-shell")
            (add-to-list 'eshell-visual-commands "ssh")
            ;; stopping the world to process file operations is insane.
            (fmakunbound 'eshell/cp)
            (fmakunbound 'eshell/mv)
            (fmakunbound 'eshell/rm)
            ;; eshell/date is inferior to GNU Coreutils date(1)
            (fmakunbound 'eshell/date)))

;;;; eww
(setq eww-download-directory (expand-file-name "~/downloads"))
(setq eww-header-line-format nil)
(setq eww-search-prefix "https://duckduckgo.com/lite/?k1=-1&q=")
(setq shr-cookie-policy nil)
(setq shr-discard-aria-hidden t)
(setq shr-max-image-proportion 0.6)
(setq shr-use-colors nil)
(setq shr-use-fonts nil)

;; rename eww buffers after rendering
(add-hook 'eww-after-render-hook
          (lambda ()
            (unless (string-empty-p (plist-get eww-data :title))
              (rename-buffer (plist-get eww-data :title) t))))

;; custom keybindings
(add-hook 'eww-mode-hook
          (lambda ()
            (define-key eww-link-keymap (kbd "RET") 'eww-open-in-new-buffer)
            (define-key eww-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))))

;;;; GNU/Emms
(autoload 'emms-browser "emms-browser" nil t)

(unless (file-directory-p (concat user-emacs-directory "emms"))
  (make-directory (concat user-emacs-directory "emms") t))

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

;; emms config
;; for i in ~/music/* { convert -resize 60x60 $i/cover.jpg $i/cover_small.png }
;; for i in ~/music/* { convert -resize 120x120 $i/cover.jpg $i/cover_medium.png }
(add-hook 'emms-browser-mode-hook
          (lambda ()
            (require 'emms-setup)
            (require 'emms-info)
            (emms-all)
            (emms-default-players)
            (setq emms-player-list (list emms-player-mpv)
                  emms-info-functions '(emms-info-opusinfo)
                  emms-mode-line-format "%s"
                  emms-playing-time-display-format " [%s] "
                  emms-source-file-default-directory "~/music/"
                  emms-mode-line-mode-line-function
                  'emms-mode-line-playlist-current)
            (add-to-list 'emms-player-base-format-list "opus")
            (emms-player-set emms-player-mpv 'regex
                             (apply #'emms-player-simple-regexp
                                    emms-player-base-format-list))))

;;;; Gnus
(setq gnus-directory (concat user-emacs-directory "news"))
(setq gnus-startup-file (concat user-emacs-directory "newsrc"))
(setq gnus-init-file (concat user-emacs-directory "gnus"))
(setq message-directory (concat user-emacs-directory "mail"))
(setq nnfolder-directory (concat user-emacs-directory "mail/archive"))

;; Gnus config
(setq gnus-inhibit-startup-message t)
(setq gnus-treat-display-smileys nil)

(setq gnus-sum-thread-tree-false-root "○ ")
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")
(setq gnus-sum-thread-tree-vertical "│ ")
(setq gnus-user-date-format-alist '((t . "%b %e")))
(setq gnus-summary-line-format
      "%4N %U%R%z %&user-date; %-14,14n (%4k) %B%s\n")

(setq gnus-asynchronous t)
(setq gnus-use-cache 'passive)

;; Gnus hooks
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-after-getting-new-news-hook
          'display-time-event-handler)
(add-hook 'gnus-group-mode-hook 'display-time-event-handler)

;; setup this demon *after* gnus has loaded, otherwise it does not work
(with-eval-after-load "gnus"
  (add-to-list 'gnus-secondary-select-methods
               '(nntp "news" (nntp-address "news.gwene.org")))

  (setq gnus-demon-timestep 1)
  (gnus-demon-add-handler 'gnus-demon-scan-news 60 t))

;; email
(setq gnus-select-method '(nnimap "email"
                                  (nnimap-address "mail.gandi.net")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

;; use smtp to send email
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "mail.gandi.net")
(setq smtpmail-smtp-service 587)

;; make subbed groups visible
(setq gnus-ignored-newsgroups
      "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq gnus-permanently-visible-groups
      "INBOX\\|Sent\\|archive\\|cyber")

;; copy sent emails to Sent
(setq gnus-message-archive-group "nnimap+email:Sent")
(setq gnus-gcc-mark-as-read t)

;; news
(defvar-local gnus-subscribe-groups-done nil
  "Only subscribe groups once.  Or else Gnus will NOT restart.")
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (unless gnus-subscribe-groups-done
              (mapc (lambda (x)
                      (gnus-subscribe-hierarchically x))
                    '("nntp+news:gwene.ca.jvns"
                      "nntp+news:gwene.com.blogspot.heronsperch"
                      "nntp+news:gwene.com.danluu"
                      "nntp+news:gwene.com.keithp.blog"
                      "nntp+news:gwene.com.mattcen.blog"
                      "nntp+news:gwene.com.nullprogram"
                      "nntp+news:gwene.com.sachachua.emacs-news"
                      "nntp+news:gwene.com.wordpress.microkerneldud"
                      "nntp+news:gwene.de.0pointer.blog"
                      "nntp+news:gwene.io.github.trofi"
                      "nntp+news:gwene.io.rosenzweig.blog"
                      "nntp+news:gwene.net.deftly"
                      "nntp+news:gwene.org.dreamwidth.mjg59"
                      "nntp+news:gwene.org.gentoo.blogs.mgorny"
                      "nntp+news:gwene.org.wingolog"
                      "nntp+news:gwene.website.christine.blog"))
              (setq gnus-subscribe-groups-done t))
            (message "Welcome to Gnus!")))

;;;; ido
;; buffer only, flex matching ido-mode
(setq ido-enable-flex-matching t)
(setq ido-ignore-buffers '("\\` " "^\*"))
(setq ido-max-window-height 1)
(ido-mode 'buffers)

;;;;; emoji picker using ido
;; list of emoji I'm likely to use
(defvar ido-emoji-list
  '("😊" "🤷" "🤦" "🥳" "😂" "😕" "😜" "😬" "👋" "👍" "👌" "😱"
    "🤮" "😭" "🥰" "🤤" "💃" "🍕" "☕"))

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

(global-set-key (kbd "C-c e") 'ido-emoji)

;;;; ix.io paste tool
(defun ix-io--process-response (response)
  "Process RESPONSE from ix.io"
  ;; FIXME: lmao, this is totally going to break someday
  (let ((url (nth 9 (string-lines response))))
    (kill-new url)
    (message (concat url " copied to kill ring."))))

(defun ix-io--post (data)
  "Post DATA to ix.io, and copy url response to kill-ring."
  (let ((url-request-method "POST")
        (url-request-data (concat "f:1="
                                  (url-hexify-string data))))
    (with-current-buffer (url-retrieve-synchronously "http://ix.io")
      (ix-io--process-response (buffer-string)))))

(defun ix-io-paste-buffer ()
  "Paste buffer using ix.io"
  (interactive)
  (when (yes-or-no-p "Paste buffer? ")
    (ix-io--post (buffer-substring (point-min) (point-max)))))

(defun ix-io-paste-region (start end)
  "Paste buffer using ix.io"
  (interactive "r")
  (when (yes-or-no-p "Paste region? ")
    (ix-io--post (buffer-substring start end))))

(global-set-key (kbd "C-c w b") 'ix-io-paste-buffer)
(global-set-key (kbd "C-c w r") 'ix-io-paste-region)

;;;; term/ansi-term
;; show URLs, hack for fish shell
(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode +1)
            (toggle-truncate-lines 1)))

;; please let me cut and paste, and other normal things
(add-hook 'term-load-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            ;; quoted paste
            (define-key term-raw-map (kbd "C-c C-y")
              (lambda ()
                (interactive)
                (term-send-raw-string
                 (format "\"%s\"" (current-kill 0)))))
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

(global-set-key (kbd "C-c v") 'cterm)

;;; Modes
;;;; common config for all prog-modes
(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode +1)
            (display-line-numbers-mode +1)
            (setq show-trailing-whitespace t)))

;;;; hl-nums, hl-bool, hl-todo
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[^a-zA-Z]\\(\\(\+\\|-\\|[0-9]+\.\\)?[0-9]+\\)[^a-zA-Z]"
                1 font-lock-constant-face)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[^a-zA-Z]\\(t\\|nil\\)[^a-zA-Z]"
                1 font-lock-constant-face)))))

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

;;;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 8)
            (setq c-default-style "linux")
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

;;;; lisp
(setq inferior-lisp-program "sbcl --no-userinit")
(global-set-key (kbd "C-c l") 'run-lisp)

;; passing --eval in inferior-lisp-program is broken
(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (lisp-eval-string "(require 'sb-aclrepl)")))

;;;; nix
(add-to-list 'auto-mode-alist
             '("\\.nix\\'" .
               (lambda ()
                 (require 'nix-mode)
                 (nix-mode))))

;;;; php
(add-to-list 'auto-mode-alist
             '("\\.php\\'" .
               (lambda ()
                 (require 'php-mode)
                 (php-mode)
                 (setq c-basic-offset 4)
                 (setq indent-tabs-mode nil)
                 (php-enable-psr2-coding-style))))

;;;; rust
(add-to-list 'auto-mode-alist
             '("\\.rs\\|.rlib\\'" .
               (lambda ()
                 (require 'rust-mode)
                 (rust-mode)
                 (setq rust-format-on-save t))))

;;;; scheme
(setq scheme-program-name "csi -n")
(global-set-key (kbd "C-c s") 'run-scheme)

;;; Functions
;;;; BMI
;; Function to calculate body mass index (BMI). For problems/flaws, see:
;; https://en.wikipedia.org/wiki/Body_mass_index
(defun bmi (weight height)
  "Return BMI for WEIGHT at HEIGHT."
  (let* ((height (if (< height 3)
                     height
                   (/ height 100.0))))
    (string-to-number (format "%.2f" (/ weight (* height height))))))

;;;; nick color generator for IRC
(defun irc-nick-color (nick)
  "Return a color for a given NICK."
  (let* ((color (concat "#" (substring (md5 (downcase nick)) 0 12))))
    (color-ensure-contrast-above-ratio color (face-attribute 'default :background) 7 5)))

;; if using stock emacs >= 28, if you don't like modus themes, you could
;; (load-theme 'modus-vivendi) then (disable-theme 'modus-vivendi)
(defun color-ensure-contrast-above-ratio (color bg ratio steps)
  "Ensure COLOR is above contrast RATIO for BG.

Before increasing contrast, tries inverting the color.  STEPS is
the pecent to increase by each pass."
  (if (< (modus-themes-contrast bg color) ratio)
      (let* ((inverted (color-complement-hex color)))
        (if (< (modus-themes-contrast bg inverted) ratio)
            (let* ((color (if (> (modus-themes-wcag-formula bg) 0.5)
                              (color-darken-name color steps)
                            (color-lighten-name color steps))))
              (color-ensure-contrast-above-ratio color bg ratio steps))
          inverted))
    color))

;;;; browse URL in mpv
(defun browse-url-mpv (url &optional _ignored)
  "Pass the specified URL to the \"mpv\" command.

The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "mpv" nil 0 nil url))

;;; outline this file
(setq outline-minor-mode-highlight 'override)

(defun outline-cycle-maybe ()
  "Run 'outline-cycle' if not on an outline heading."
  (interactive)
  (if (outline-on-heading-p)
      (outline-cycle)
    (indent-for-tab-command)))

(add-to-list 'safe-local-variable-values
             '(eval progn (outline-minor-mode 1) (hide-sublevels 1)))
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key
              outline-minor-mode-map (kbd "<tab>") 'outline-cycle-maybe)))

;; Local Variables:
;; outline-regexp: ";;; \\|;;;; \\|;;;;;"
;; eval:(progn (outline-minor-mode 1) (hide-sublevels 1))
;; End:

;;; end initialisation
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))

(provide 'init)
;;; init.el ends here
