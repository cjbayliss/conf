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
(setq browse-url-handlers
      `((,(rx "youtu" (? "." ) "be" (* anything) "watch") . browse-url-mpv)
        ("." . browse-url-firefox)))
(setq c-basic-offset 4)
(setq column-number-mode t)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq help-window-select t)
(setq make-backup-files nil)
(setq org-cycle-include-plain-lists 'integrate)
(setq package--init-file-ensured t)
(setq require-final-newline t)
(setq shell-file-name "sh")
(setq split-width-threshold 158)
(setq tab-always-indent 'complete)
(setq tramp-allow-unsafe-temporary-files t)
(setq-default fill-column 72)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)

;;;; misc options/changes
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)

;; upcase-region & downcase-region are disabled, but not this. WTF??
(fmakunbound 'overwrite-mode)

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
            (when (fboundp 'pinentry-start)
              (pinentry-start))
            (when (fboundp 'marginalia-mode)
              (marginalia-mode +1))
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
    (set-frame-font "Iosevka Fixed-10" 'keep-size t))
  ;; 😜
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))
  ;; 한국어/조선말
  (when (member "Baekmuk Gulim" (font-family-list))
    (set-fontset-font t 'unicode "Baekmuk Gulim-10" nil 'prepend))
  ;; 日本語
  (when (member "IPAGothic" (font-family-list))
    (set-fontset-font t 'unicode "IPAGothic-10" nil 'prepend))
  ;; Latin/Cyrillic
  (when (member "Iosevka Fixed" (font-family-list))
    (set-fontset-font t 'unicode "Iosevka Fixed-10" nil 'prepend)))

;;;; setup GUI only stuff
(when (display-graphic-p)
  (setq mouse-yank-at-point t)
  (setq x-gtk-use-system-tooltips nil)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-face-attribute 'mode-line nil :inherit 'default)
  (server-start))

;;;; keybindings
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c m") 'proced)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c p") 'run-python)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 🤬
(global-unset-key (kbd "C-z"))

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
      " "
      (:eval (when (boundp 'rcirc-activity)
               (when rcirc-activity
                 rcirc-activity-string)))
      " "
      (:eval (format-mode-line 'mode-line-modes 'font-lock-doc-face))
      (:eval (format-mode-line '(" " display-time-string) 'bold))
      "  "
      (:eval (format-mode-line mode-line-misc-info
                               'font-lock-comment-delimiter-face))
      mode-line-end-spaces))

   (display-time-mode +1)
   (delq 'display-time-string global-mode-string)))

;;; Tools
;;;; dired
(setq dired-listing-switches "-ABlhFv")
(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; quit means quit, please!
            (define-key dired-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))))

;;;; elfeed
(unless (file-directory-p (concat user-emacs-directory "elfeed"))
  (make-directory (concat user-emacs-directory "elfeed") t))

(setq elfeed-db-directory (concat user-emacs-directory "elfeed"))
(setq elfeed-search-filter "+unread")

(setq elfeed-feeds
      '(("https://0pointer.net/blog/index.rss20" blog)
        ("https://blog.jeff.over.bz/rss.xml" blog)
        ("https://blog.mattcen.com/rss" blog)
        ("https://blogs.gentoo.org/mgorny/feed/" blog)
        ("https://blogs.igalia.com/apinheiro/feed/" blog)
        ("https://blogs.igalia.com/dpiliaiev/feed.xml" blog)
        ("https://christine.website/blog.rss" blog)
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
        ("https://wingolog.org/feed/atom" blog guile)

        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-RA5BzE_BnZhf5iVdNF1hA" youtube video) ; loopop
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2QjlMMSdO2aITpYlwdqZQw" youtube video) ; Wirtual Clips
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5jnkxwILkgNePRe1FacHzw" youtube video) ; 山崎まさよし
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD6v_eY0IDEGittgHsmd8aQ" youtube video) ; e r g o j o s h
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFvfgkNeSgNXQWcxBIOhSPw" youtube video) ; Marshall Fox
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCG44gzf6Iumy18XEZXT2odA" youtube video) ; Red Means Recording Ambient
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCHDes_67fEXHzUnV6CxiDkA" youtube video) ; GranaDy
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJflT7i52L1ZFkdvKyFCHtQ" youtube video) ; Hefest
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJwM0fiKe2rq7z2p8HPTyMA" youtube video) ; Zepla HQ
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCNMn4Nhl-E3iaSMab0Y_b4A" youtube video) ; wacci OFFICIAL YouTube CHANNEL
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPNq-cWaMPccZydtyhMXGGQ" youtube video) ; Michael Klements
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQVhrypJhw1HxuRV4gX6hoQ" youtube video) ; あいみょん
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQhvDZeUrxPq9p3SkbTngkA" youtube video) ; TÂCHES TEACHES
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRIgIJQWuBJ0Cv_VlU3USNA" youtube video) ; ヨルシカ / n-buna Official
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVN95kHPHk--fSwvTCRJmdw" youtube video) ; Jakub Ciupinski
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXCXxhRVYvBOX45_gxr0iHA" youtube video) ; Christian Henson Music
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCafxR2HWJRmMfSdyZXvZMTw" youtube video) ; LOOK MUM NO COMPUTER
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcUcK64JLSZAPUfG07s-Wew" youtube video) ; 宮本浩次
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcXBraaImQrLrBuXTk6L2cQ" youtube video) ; 岡ちゃんnel
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdcemy56JtVTrsFIOoqvV8g" youtube video) ; ANDREW HUANG
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCf-vV5woXPFpkvZKwooWoyw" youtube video) ; WirtualTV
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCfhW84xfA6gEc4hDK90rR1Q" youtube video) ; Thomas Heaton
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkB8HnJSDSJ2hkLQFUc-YrQ" youtube video) ; King Gnu
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkjbN2BlTwm7XknNjLOj7Hg" youtube video) ; subtractem
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCld4DwWufxVee57cIrf9ERg" youtube video) ; Craig Loewen
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmaec75I8GbMci0_IFDq7ig" youtube video) ; リュックと添い寝ごはん
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCn8KRTJhyH8AatY10-BbxNg" youtube video) ; Nervyr
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCogpH3RuCIvNlCXz9ocMK9Q" youtube video) ; Mocha
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoycwm1Mc9FMtzi0hynKlNA" youtube video) ; 折坂悠太
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpjl7axNrHYPh53gezouYxA" youtube video) ; Shane Mendonsa
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCshObcm-nLhbu8MY50EZ5Ng" youtube video) ; Benn Jordan
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg" youtube video) ; Wolfgang's Channel
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCt-HTfaCUz8QIoknqyXKYiw" youtube video) ; Wirtual
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCuWKHSHTHMV_nVSeNH4gYAg" youtube video) ; Omri Cohen
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCx6GR83AbXiSX2gesCxSPPw" youtube video) ; Empyriangaming
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyDZai57BfE_N0SaBkKQyXg" youtube video) ; Rob Scallon
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzpaOq5_NbX5KnkIEByiggA" youtube video) ; GranaDy & Friends
        ))

;;;; eshell
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 4096)
(setq eshell-input-filter 'eshell-input-filter-initial-space)
(setq eshell-ls-initial-args "-h")
(setq eshell-scroll-to-bottom-on-input 'all)

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

;;;; etags
(defun ctags-scan-dir (directory)
  "Generate a TAGS file in DIRECTORY using universal-ctags."
  (call-process-shell-command
   (format "ctags -f %sTAGS -e -R %s" directory directory)))

(defun vc-generate-etags ()
  "Generate a TAGS file in the vc-root-dir."
  (interactive)
  (let ((root (vc-root-dir)))
    (or (when root
          (message (format "Generating TAGS file for %s ..." root))
          (ctags-scan-dir root)
          (message (format "Generating TAGS file for %s ... Done." root)))
        (message "Can't find the version control root directory."))))

(define-key global-map (kbd "C-x v t") 'vc-generate-etags)

;;;; GNU/Emms
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

;;;; icomplete
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; use completing-read for completion in region
(setq completion-in-region-function
      #'completing-read-completion--in-region)

(setq icomplete-compute-delay 0)
(setq icomplete-scroll t)
(setq icomplete-show-matches-on-no-input t)

(icomplete-vertical-mode +1)
(icomplete-mode +1)

;; without truncate-lines, icomplete-vertical spews a mess
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (setq-local truncate-lines t)))

(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
(define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)

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

;;;; rcirc
(with-eval-after-load 'rcirc

;;;;; rcirc functions
  ;; BEGIN GPL2+ CODE FROM:
  ;; https://github.com/emacsmirror/rcirc-color/blob/7d9655e/rcirc-color.el
  (defvar rcirc-color-mapping (make-hash-table :test 'equal))

  (defun rcirc-clear-color-mapping ()
    (setq rcirc-color-mapping (make-hash-table :test 'equal)))

  (advice-add 'rcirc-facify :around #'rcirc-color--facify)
  (defun rcirc-color--facify (orig-fun string face &rest args)
    "Add colors to other nicks based on `rcirc-colors'."
    (when (and (eq face 'rcirc-other-nick)
               (> (length string) 0))
      (let ((cell (or (gethash string rcirc-color-mapping)
                      (puthash (substring-no-properties string)
                               `(:foreground
			         ,(irc-nick-color string))
                               rcirc-color-mapping))))
        (setq face (list cell))))
    (apply orig-fun string face args))

  (defun rcirc-markup-nick-colors (_sender _response)
    "Add a face to all known nicks in `rcirc-color-mapping'.
This ignores SENDER and RESPONSE."
    (with-syntax-table rcirc-nick-syntax-table
      (while (re-search-forward "\\w+" nil t)
        (let ((face (gethash (match-string-no-properties 0) rcirc-color-mapping)))
	  (when face
	    (rcirc-add-face (match-beginning 0) (match-end 0) face))))))

  (add-hook 'rcirc-markup-text-functions #'rcirc-markup-nick-colors)
  ;; END GPL2+ CODE ;;

  (require 'notifications)
  (defun rcirc-notifications (process sender response target text)
    (when (or (and (string= response "PRIVMSG")
                   (not (string= sender (rcirc-nick process)))
                   (not (rcirc-channel-p target)))
              (and (string-match (rcirc-nick process) text)
                   (rcirc-channel-p target)
                   (not (string= (rcirc-nick process) sender))
                   (not (string= (rcirc-server-name process) sender))))
      (notifications-notify :app-icon nil :title sender :body text)))

;;;;; rcirc hooks
  (add-hook 'rcirc-mode-hook (lambda ()
                               (rcirc-omit-mode +1)
                               (rcirc-track-minor-mode  +1)
                               (flyspell-mode +1)
                               (setq-local fill-column (frame-width))
                               (setq-local completion-in-region-function #'completion--in-region)
                               (set-face-attribute 'rcirc-nick-in-message-full-line nil :foreground nil :weight 'normal)
                               (set-face-attribute 'rcirc-my-nick nil :foreground nil :weight 'normal :inherit 'rcirc-nick-in-message)
                               (set (make-local-variable 'scroll-conservatively) 8192)))

  (add-hook 'rcirc-track-minor-mode-hook (lambda ()
                                           (delq 'rcirc-activity-string global-mode-string)))

  (add-hook 'rcirc-print-hooks 'rcirc-notifications)

;;;;; rcirc config
  (setq rcirc-default-full-name "Christopher Bayliss (they/them)")
  (setq rcirc-default-nick "cjbayliss")
  (setq rcirc-default-user-name "cjbayliss")
  (setq rcirc-fill-column 'frame-width)
  (setq rcirc-buffer-maximum-lines 2048)
  (setq rcirc-ignore-list '("{^-^}"
                            "Hash"))
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/stuff/rcirc-log")
  ;; (setq rcirc-debug-flag t)
  (setq rcirc-server-alist '(("irc.au.libera.chat"
                              :port 6697
                              :encryption tls
                              :nick "cjb"
                              :user-name "cjb"
                              :server-alias "libera"
                              :channels ("#chicken"
                                         "#commonlisp"
                                         "#fennel"
                                         "#haskell"
                                         "#lisp"
                                         "##math"
                                         "#nixos"
                                         "#python"
                                         "#rcirc"
                                         "#scheme"
                                         "#xebian"))
                             ("irc.oftc.net"
                              :port 6697
                              :encryption tls
                              :nick "cjbayliss"
                              :user-name "cjbayliss"
                              :server-alias "oftc"
                              :channels ("#llvm"))))

  (setq rcirc-authinfo
        `(("libera" sasl "cjb" ,(auth-source-pass-get 'secret "irc.libera.chat"))
          ("oftc" nickserv "cjbayliss" ,(auth-source-pass-get 'secret "irc.oftc.net")))))

(defun irc-cyber ()
  (interactive)
  (rcirc-connect "127.0.0.1" 6667 "cjb" nil "Christopher Bayliss (they/them)" '("#cyber") "cyber"))

;;;; term/ansi-term
;; show URLs
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local kill-read-only-ok t)
            (goto-address-mode +1)))

;; please let me cut and paste, and other normal things
(add-hook 'term-load-hook
          (lambda ()
            (define-key term-raw-map (kbd "M-:") 'eval-expression)
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-k")
              (lambda ()
                (interactive)
                (term-send-raw-string "\C-k")
                (kill-line)))))

;; always kill-buffer after exit
(advice-add 'term-handle-exit :filter-return #'kill-buffer)

(global-set-key (kbd "C-c v")
                (lambda ()
                  (interactive)
                  (if (get-buffer "*ansi-term*")
                      (switch-to-buffer "*ansi-term*")
                    (ansi-term "/run/current-system/sw/bin/bash"))))

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
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|IMPORTANT\\):"
                1 highlight-todo-face t)))))

;;;; tree-sitter
(add-hook
 'after-init-hook
 (lambda ()
   ;; prevent emacs from flickering at startup
   (let ((inhibit-message t))
     ;; set the default dir to ~/.config/emacs/tree-sitter
     (setq tree-sitter-langs-grammar-dir
           (expand-file-name "tree-sitter/" user-emacs-directory))
     (setq tsc-dyn-dir tree-sitter-langs-grammar-dir)
     (make-directory tree-sitter-langs-grammar-dir t)

     ;; auto-install grammar files
     (tree-sitter-langs-install-grammars t)

     ;; IMPORTANT: the grammar alist isn't generated unless you require
     ;; this first
     (require 'tree-sitter-langs)

     ;; now enable tree-sitter
     (global-tree-sitter-mode)
     (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))))

;;;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 8)
            (setq c-default-style "linux")
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

;;;; lisp
(setq inferior-lisp-program
      "sbcl --no-userinit --eval \"(require 'sb-aclrepl)\"")
(global-set-key (kbd "C-c l") 'run-lisp)

;;;; nix
(add-to-list 'auto-mode-alist
             '("\\.nix\\'" .
               (lambda ()
                 (require 'nix-mode)
                 (nix-mode))))

;;;; php
(add-to-list
 'auto-mode-alist
 '("\\.php\\'" .
   (lambda ()
     (defun podman-build-allocPSA ()
       "Build an allocPSA pod."
       (interactive)
       (async-shell-command
        (format "podman build -f $HOME/stuff/podman/alloc-containerfile %s -t alloc"
                (expand-file-name (vc-root-dir)))
        "Podman: Building allocPSA"))

     (defun podman-run-allocPSA ()
       "Run an allocPSA pod."
       (interactive)
       (async-shell-command
        (format "podman run --name alloc --volume %s:/var/www/html -p 4000:80 alloc"
                (expand-file-name (vc-root-dir)))
        "Podman: Running allocPSA"))

     (defun podman-stop-allocPSA ()
       "Stop the running allocPSA pod."
       (interactive)
       (message "Podman: Stopping allocPSA ...")
       (call-process-shell-command "podman stop alloc")
       (message "Podman: Stopping allocPSA ... Done.")
       ;; cleanup the mess
       (call-process-shell-command "podman system prune -f"))

     (require 'php-mode)
     (php-mode)
     (define-key php-mode-map (kbd "C-c a b") 'podman-build-allocPSA)
     (define-key php-mode-map (kbd "C-c a r") 'podman-run-allocPSA)
     (define-key php-mode-map (kbd "C-c a s") 'podman-stop-allocPSA)
     (setq c-basic-offset 4)
     (setq indent-tabs-mode nil)
     (php-enable-psr2-coding-style))))

;;;; scheme
(setq scheme-program-name "csi")
(global-set-key (kbd "C-c s") 'run-scheme)

;; in eshell comments aren't highlighted. In run-scheme CHICKEN Scheme's
;; prompt gets incorrectly highlighted as a comment *after* the face to
;; highlight it as a prompt is set. This ensures run-scheme's prompt
;; uses `comint-highlight-prompt'.
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (setq-local font-lock-comment-face nil)))

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

;;;; completion-in-region-function
(defun completing-read-completion--in-region (start end coll &optional pred)
  "Use 'completing-read' to select a completion in region.

START and END are the position in the buffer of the string to be
completed.  COLL is the collection of possible completions, and
PRED limits the possible completions to a subset of COLL.

The following are tried in order:

 - in a minibuffer, run the 'completion--in-region' function

 - if there is only one possible completion, insert it

 - run 'completing-read' with a list of possible completions, and
   insert that."
  (if (minibufferp)
      (completion--in-region start end coll pred)
    (let* ((init (buffer-substring-no-properties start end))
           (comp (all-completions init coll pred))
           (meta (completion-metadata-get
                  (completion-metadata init coll pred) 'category)))
      (pcase (safe-length comp)
        (`0)
        (`1 (let ((minibuffer-completion-table coll)
                  (minibuffer-completion-predicate pred))
              (completion--in-region-1 start end)))
        ( _ (let ((sel
                   (if (eq meta 'file)
                       (replace-regexp-in-string
                        "^\\\\~/" "~/"
                        (replace-regexp-in-string
                         "\\\\ $" " "
                         (shell-quote-argument
                          (read-file-name "Completions: "
                                          (file-name-directory init) init nil
                                          (file-name-nondirectory init) pred))))
                     (completing-read "Completions: " coll pred nil init))))
              (when sel
                (completion--replace start end sel))))))))

;;; outline this file
(setq outline-minor-mode-highlight 'override)

(defun outline-cycle-maybe ()
  "Run 'outline-cycle' if on an outline heading."
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
