;; there may be strange approaches in this conifg, here are two reasons why:
;;
;;     1. i'm an idiot who doesn't know elisp.
;;
;;     2. it's faster, and keeps my emacs-init-time below 0.2s on my 11 year
;;     old Macbook. (current init time 2019-04-27: 0.0s)

;; general emacs settings
(setq inhibit-startup-screen t
      column-number-mode t
      make-backup-files nil
      require-final-newline t
      c-basic-offset 4
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
      ;; get stuff out of the home dir
      gnus-directory "~/.emacs.d/news/"
      gnus-startup-file "~/.emacs.d/newsrc"
      gnus-init-file "~/.emacs.d/gnus"
      ;; let the game begin! (people have heated views on this setting)
      gc-cons-threshold 100
      initial-scratch-message ";; ┌─┐┌┐┬┬ ┬ ┌─┐┌┬┐┌─┐┌─┐┌─┐
;; │┌┐│└┤│ │ ├┤ │││├─┤│  └─┐
;; └─┘┴ ┴└─┘ └─┘┴ ┴┴ ┴└─┘└─┘
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")

(setq-default fill-column 79
              frame-background-mode 'dark
              indent-tabs-mode nil
              show-trailing-whitespace t)

;; enable/disable modes
(show-paren-mode +1)
(delete-selection-mode +1)
(menu-bar-mode -1)

;; yes, these modes slow down startup a lot 😢
(save-place-mode +1)
(global-hl-line-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-cl" 'display-line-numbers-mode)
(global-set-key "\C-ch" 'hl-line-mode)

;; see https://github.com/jorgenschaefer/circe/wiki/Configuration
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

;; custom irc func to load erc and join networks automatcially
(defun irc ()
  "Connect to IRC."
  (interactive)
  (require 'circe)
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)

  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)
  (global-hl-line-mode -1)

  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix "      "))

  (setq lui-fill-type nil
        lui-time-stamp-position 'left
        lui-time-stamp-format "%H:%M "
        lui-time-stamp-only-when-changed-p nil
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
           :host "irc.oftc.net"
           :nick "cjbayliss"
           :nickserv-password my/return-password
           :channels (:after-auth "#debian-au" "#debian-devel" "#debian-next" "#debian-mentors" "#debian-next" "#packaging"))
          ("Cyber"
           :host "127.0.0.1"
           :port "6667"
           :nick "cjb"
           :channels ("#cyber"))
          ("Freenode"
           :host "chat.au.freenode.net"
           :nick "cjb"
           :sasl-username "cjb"
           :sasl-password my/return-password
           :nickserv-password my/return-password
           :channels (:after-auth "#xebian" "#gentoo" "#emacs" "#python" "#allocpsa" "#lisp" "#clojure" "##asm" "##c"))))
  (circe "Cyber")
  (circe "OFTC")
  (circe "Freenode")

  (custom-set-faces '(circe-my-message-face ((t (:foreground "color-216"))))
                    '(circe-prompt-face ((t (:foreground "cyan1"))))
                    '(circe-server-face ((t (:foreground "chocolate1"))))
                    '(lui-time-stamp-face ((t (:foreground "brightwhite")))))

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what life it
  ;; has lived, but since you know how to use git you'll find out in no time!!
  ;; (yes, I felt like writing about this paren for no reason at all.)
  )

;; add packages to load path
(let ((default-directory  "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; load php stuff grumble grumble
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (autoload 'php-mode "php-mode")
                               (php-mode)
                               (setq c-basic-offset 4)
                               (php-enable-psr2-coding-style))))

;; load nasm-mode instead of the broken(?) asm-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(asm\\|nasm\\|S\\|s\\)\\'" . (lambda ()
                                                    (load "~/.emacs.d/nasm-mode")
                                                    (nasm-mode))))

;; if I'm editing a C file, I *probably* want the linux style
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux"
                  c-basic-offset 8
                  tab-width 8
                  indent-tabs-mode t)))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(line-number-current-line ((t (:background "darkolivegreen" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1")))))
