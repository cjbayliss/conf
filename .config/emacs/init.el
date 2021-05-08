;;; Init --- GNU Emacs initialisation file

;; Author: Christopher Bayliss <cjb@cjb.sh>
;; Created: 2021-05-05
;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Complete rewrite of my old Emacs initialisation file.

;; Raison d'être: readability > correctness > speed.

;;; Code:

;;;; Beginning of initialisation
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; load ensure-pkg
(load (concat user-emacs-directory "lisp/ensure-pkg") nil nil)

;;;; Setup modus themes
(ensure-pkg 'modus-themes "https://gitlab.com/protesilaos/modus-themes")

(require 'modus-themes)
(setq modus-themes-slanted-constructs t)
(setq modus-themes-no-mixed-fonts t)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

;;;; General Emacs configuration
(setq browse-url-browser-function 'browse-url-firefox)
(setq c-basic-offset 4)
(setq column-number-mode t)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq make-backup-files nil)
(setq package--init-file-ensured t)
(setq require-final-newline t)

(setq-default fill-column 72)		; 72 is better for my display
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)

(menu-bar-mode -1)
(save-place-mode +1)

;; these modes are slow to load, add them to this hook instead.
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

(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable/change startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message (concat "In the beginning the Emacs was created. This has "
                   "made a lot of people very angry and been widely "
                   "regarded as a bad move.")))

;; don't popup the async-shell-command buffer
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*"
                   (cons #'display-buffer-no-window nil)))

;;;; Setup fonts
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

;;;; Setup GUI only stuff
(when (display-graphic-p)
  (setq mouse-yank-at-point t)
  (setq x-gtk-use-system-tooltips nil)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (server-start))

;;;; Keybindings
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c l") 'run-lisp)
(global-set-key (kbd "C-c m") 'proced)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c p") 'run-python)
(global-set-key (kbd "C-c s") 'run-scheme)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;; Ido
(setq ido-enable-flex-matching t)
(setq ido-ignore-buffers '("\\` " "^\*"))
(ido-mode 'buffers)                     ; only for buffer switching

;;;; Which-key
(ensure-pkg 'which-key "https://github.com/justbur/emacs-which-key")

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'which-key)
            (which-key-mode)))

;;;; Ido-emoji
;; list of emoji I'm likely to use
(defvar ido-emoji-list
  '("🙂" "🤷" "🤦" "🥳" "🤣" "🤨" "😜" "😬" "👋" "👍" "👌" "😱"
    "🤮" "😭" "😑" "💃"))

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

;;;; Circe
(ensure-pkg 'circe "https://github.com/jorgenschaefer/circe")
(ensure-pkg 'erc-hl-nicks "https://github.com/leathekd/erc-hl-nicks")

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

;;;; Dired
(ensure-pkg 'diredfl "https://github.com/purcell/diredfl")

(setq dired-listing-switches "-ABlhF")

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
              (lambda () (interactive) (quit-window t)))
            ;; nice colouring in dired
            (require 'diredfl)
            (diredfl-mode)))

;;;; Term/ansi-term
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

;;;; Eshell
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 4096)
(setq eshell-input-filter 'eshell-input-filter-initial-space)
(setq eshell-ls-initial-args "-h")
(setq eshell-scroll-to-bottom-on-input 'all)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "cat")
            ;; stopping the world to process file operations is insane.
            (fmakunbound 'eshell/cp)
            (fmakunbound 'eshell/mv)
            (fmakunbound 'eshell/rm)
            ;; eshell/date is inferior to GNU Coreutils date(1)
            (fmakunbound 'eshell/date)))

;;;; Eww
(setq eww-download-directory (expand-file-name "~/downloads"))
(setq eww-header-line-format nil)
(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq shr-cookie-policy nil)
(setq shr-discard-aria-hidden t)
(setq shr-max-image-proportion 0.6)
(setq shr-use-colors nil)
(setq shr-use-fonts nil)

;;;; Elpher
(ensure-pkg 'elpher "git://thelambdalab.xyz/elpher.git")
(autoload 'elpher "elpher" nil t)
(setq elpher-ipv4-always t)

;;;; Elfeed
(ensure-pkg 'elfeed "https://github.com/skeeto/elfeed")
(autoload 'elfeed "elfeed" nil t)

(unless (file-directory-p (concat user-emacs-directory "elfeed"))
  (make-directory (concat user-emacs-directory "elfeed") t))

(setq elfeed-db-directory (concat user-emacs-directory "elfeed"))
(setq elfeed-search-filter "+blog +unread")

(setq elfeed-feeds
      '(("https://0pointer.net/blog/index.rss20" blog)
        ("https://blog.alicef.me/feeds/all.atom.xml" blog)
        ("https://blog.jeff.over.bz/rss.xml" blog)
        ("https://blog.mattcen.com/rss" blog)
        ("https://blogs.gentoo.org/mgorny/feed/" blog)
        ("https://blogs.igalia.com/apinheiro/feed/" blog)
        ("https://blogs.igalia.com/dpiliaiev/feed.xml" blog)
        ("https://christine.website/blog.rss" blog)
        ("https://codingquark.com/feed.xml" blog)
        ("https://danluu.com/atom.xml" blog)
        ("https://deftly.net/rss.xml" blog)
        ("https://heronsperch.blogspot.com/feeds/posts/default?alt=rss" blog)
        ("https://jvns.ca/atom.xml" blog)
        ("https://keithp.com/blogs/index.rss" blog)
        ("https://melissawen.github.io/feed.xml" blog)
        ("https://microkerneldude.wordpress.com/feed/" blog)
        ("https://mjg59.dreamwidth.org/data/rss" blog)
        ("https://nullprogram.com/feed/" blog)
        ("https://rosenzweig.io/blog/feed.xml" blog)
        ("https://sachachua.com/blog/category/emacs-news/feed" blog emacs)
        ("https://trofi.github.io/feed/rss.xml" blog)
        ("https://wingolog.org/feed/atom" blog guile)))

;;;; GNU/Emms
(ensure-pkg 'emms "https://git.savannah.gnu.org/git/emms.git")
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
;; FIXME: cleanup Gnus config.
(setq gnus-directory (concat user-emacs-directory "news"))
(setq gnus-startup-file (concat user-emacs-directory "newsrc"))
(setq gnus-init-file (concat user-emacs-directory "gnus"))

(setq
 gnus-select-method '(nnimap "email"
                             (nnimap-address "mail.gandi.net")
                             (nnimap-server-port 993)
                             (nnimap-stream ssl))

 ;; modified from: http://cyber.com.au/~twb/.emacs
 gnus-sum-thread-tree-false-root "──○ "
 gnus-sum-thread-tree-indent "  "
 gnus-sum-thread-tree-leaf-with-other "├─● "
 gnus-sum-thread-tree-root "■ "
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-single-leaf "╰─● "
 gnus-sum-thread-tree-vertical "│ "
 gnus-user-date-format-alist '((t . "%b %e"))
 gnus-summary-line-format "%4N %U%R%z %&user-date; %-14,14n (%4k) %B%s\n"

 ;; use smtp to send email
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "mail.gandi.net"
 smtpmail-smtp-service 587

 ;; make subbed groups visible
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
 gnus-permanently-visible-groups "INBOX\\|Sent\\|archive\\|cyber"
 gnus-asynchronous t
 gnus-use-cache 'passive

 ;; copy sent emails to Sent
 gnus-message-archive-group "nnimap+email:Sent"
 message-directory (concat user-emacs-directory "mail")
 nnfolder-directory (concat user-emacs-directory "mail/archive")
 gnus-gcc-mark-as-read t)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-after-getting-new-news-hook
          'display-time-event-handler)
(add-hook 'gnus-group-mode-hook 'display-time-event-handler)

;; setup this demon *after* gnus has loaded, otherwise it does not work
(with-eval-after-load "gnus"
  (setq gnus-demon-timestep 1)
  (gnus-demon-add-handler 'gnus-demon-scan-news 60 t))

;;;; Webpaste
(ensure-pkg 'request "https://github.com/tkf/emacs-request")
(ensure-pkg 'webpaste "https://github.com/etu/webpaste.el")

(autoload 'webpaste-paste-buffer "webpaste" nil t)
(autoload 'webpaste-paste-region "webpaste" nil t)

(setq webpaste-paste-confirmation t)
(setq webpaste-provider-priority '("paste.rs" "bpa.st" "dpaste.org"))

(global-set-key (kbd "C-c w b") 'webpaste-paste-buffer)
(global-set-key (kbd "C-c w r") 'webpaste-paste-region)

;;;; Version Control
(ensure-pkg 'popup "https://github.com/auto-complete/popup-el")
(ensure-pkg 'vc-msg "https://github.com/redguardtoo/vc-msg")
(autoload 'vc-msg-show "vc-msg" nil t)
(global-set-key (kbd "C-c /") 'vc-msg-show)

;;;; Programming modes
;; common config for all prog-modes
(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode +1)
            (display-line-numbers-mode +1)
            (setq show-trailing-whitespace t)))

;; highlight numbers in all prog-modes
(ensure-pkg 'parent-mode "https://github.com/Fanael/parent-mode")
(ensure-pkg 'highlight-numbers "https://github.com/Fanael/highlight-numbers")
(add-hook 'prog-mode-hook
          (lambda ()
            (require 'highlight-numbers)
            (highlight-numbers-mode +1)))

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

;; php
(ensure-pkg 'php-mode "https://github.com/emacs-php/php-mode" "/lisp")
(add-to-list 'auto-mode-alist
             '("\\.php\\'" .
               (lambda ()
                 (require 'php-mode)
                 (php-mode)
                 (setq c-basic-offset 4)
                 (setq indent-tabs-mode nil)
                 (php-enable-psr2-coding-style))))

;; rust
(ensure-pkg 'rust-mode "https://github.com/rust-lang/rust-mode")
(add-to-list 'auto-mode-alist
             '("\\.rs\\|.rlib\\'" .
               (lambda ()
                 (require 'rust-mode)
                 (rust-mode)
                 (setq rust-format-on-save t))))

;; nix
(ensure-pkg 'mmm-mode "https://github.com/purcell/mmm-mode")
(ensure-pkg 'nix-mode "https://github.com/NixOS/nix-mode")
(add-to-list 'auto-mode-alist
             '("\\.nix\\'" .
               (lambda ()
                 (require 'nix-mode)
                 (nix-mode))))

;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 8)
            (setq c-default-style "linux")
            (setq indent-tabs-mode t)
            (setq tab-width 8)))


;;;; Lisp and scheme
(setq inferior-lisp-program "sbcl --no-userinit")
(setq scheme-program-name "csi -n")

;; passing --eval in inferior-lisp-program is broken
(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (lisp-eval-string "(require 'sb-aclrepl)")))

;;;; Setup the mode-line
;; configuring the mode-line is pretty ugly 🤮
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

;;;; Outline this file
(add-to-list 'safe-local-variable-values
             '(eval progn (outline-minor-mode 1) (hide-body)))
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key
              outline-minor-mode-map (kbd "H-s-SPC") 'outline-cycle)))
;; Local Variables:
;; outline-regexp: ";;; \\|;;;; "
;; eval:(progn (outline-minor-mode 1) (hide-body))
;; End:

;;;; End initialisation
(setq gc-cons-threshold 800000)
(setq gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
