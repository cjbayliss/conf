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
      ;; why does this need to be set here?
      gnus-directory "~/.emacs.d/news/"
      ;; let the game begin! (people have heated views on this setting)
      gc-cons-threshold 100
      initial-scratch-message ";; ┌─┐┌┐┬┬ ┬ ┌─┐┌┬┐┌─┐┌─┐┌─┐
;; │┌┐│└┤│ │ ├┤ │││├─┤│  └─┐
;; └─┘┴ ┴└─┘ └─┘┴ ┴┴ ┴└─┘└─┘
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")

(setq-default fill-column 79
              save-place t
              frame-background-mode 'dark
              indent-tabs-mode nil
              show-trailing-whitespace t)

;; enable some modes.
(show-paren-mode +1)
(delete-selection-mode +1)
(save-place-mode +1)

;; disable some modes
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-cl" 'display-line-numbers-mode)

;; custom irc func to load erc and join networks automatcially
(defun irc ()
  "Connect to IRC."
  (interactive)

  ;; set this here, the auto resize is below
  (setq erc-fill-column 157)
  (erc-services-mode +1)

  ;; load erc-sasl
  (add-to-list 'load-path "~/.emacs.d/elisp/erc-sasl")
  (require 'erc-sasl)

  ;; redefine the erc-login function so erc-sasl works
  (defun erc-login ()
    "Perform user authentication at the IRC server."
    (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		     (erc-current-nick)
		     (user-login-name)
		     (or erc-system-name (system-name))
		     erc-session-server
		     erc-session-user-full-name))
    (if erc-session-password
	(erc-server-send (format "PASS %s" erc-session-password))
      (message "Logging in without password"))
    (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
      (erc-server-send "CAP REQ :sasl"))
    (erc-server-send (format "NICK %s" (erc-current-nick)))
    (erc-server-send
     (format "USER %s %s %s :%s"
	     ;; hacked - S.B.
	     (if erc-anonymous-login erc-email-userid (user-login-name))
	     "0" "*"
	     erc-session-user-full-name))
    (erc-update-mode-line))

  ;; use erc-sasl for freenode
  (add-to-list 'erc-sasl-server-regexp-list "chat\\.au\\.freenode\\.net")

  (erc-tls :server "chat.au.freenode.net" :port 6697 :nick "cjb" :full-name "Christopher Bayliss")
  (erc-tls :server "irc.oftc.net" :port 6697 :nick "cjbayliss" :full-name "Christopher Bayliss"))

;; ERC config
(with-eval-after-load "erc"
  (autoload 'erc-goodies "erc-goodies")

  (setq erc-prompt-for-password nil
        erc-autojoin-timing 'ident
        erc-rename-buffers t
        erc-track-exclude-server-buffer t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-server-reconnect-timeout 60
        erc-autojoin-channels-alist
        '(("freenode.net" "#xebian" "#emacs" "#allocpsa" "#stumpwm")
          ("oftc.net" "#debian-devel" "#debian-next" "#debian-mentors"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)
  ;; load erc-hl-nicks
  (load "~/.emacs.d/elisp/erc-hl-nicks/erc-hl-nicks")
  (erc-hl-nicks)
  (erc-scrolltobottom-enable)
  (erc-notifications-mode +1)
  (erc-spelling-mode +1)

  ;; make ERC use full buffer width
  (add-to-list 'window-configuration-change-hook
               (lambda () (setq erc-fill-column (- (window-width) 2))))
  ;; keep ERC buffer pined to bottom
  (add-to-list 'erc-mode-hook
               (lambda () (set (make-local-variable 'scroll-conservatively) 100)))

  ;; fix ERC prompt colors
  (custom-set-faces '(erc-prompt-face ((t (:foreground "brightwhite" :background nil :weight bold))))
                    '(erc-input-face ((t (:foreground "white")))))

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what life it
  ;; has lived, but since you know how to use git you'll find out in no time!!
  ;; (yes, I felt like writing about this paren for no reason at all.)
  )

;; php-mode config
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (load "~/.emacs.d/elisp/php-mode/php-project")
                               (load "~/.emacs.d/elisp/php-mode/php-mode")
                               (php-mode)
                               (setq c-basic-offset 4)
                               (php-enable-psr2-coding-style))))

;; d-mode
(add-to-list 'auto-mode-alist
             '("\\.d\\'" . (lambda ()
                             (load "~/.emacs.d/elisp/d-mode/d-mode")
                             (d-mode)
                             (setq c-basic-offset 4))))

;; I previouly considered this section to be yuk. It still is. But there are
;; worse ways.
(defun my/get-sha512sum (file)
  "get sha512sum of file"
  (replace-regexp-in-string " .+$" "" (shell-command-to-string (concat "sha512sum -z " file))))

(defun my/wget (file dest)
  "download a file with wget because url-copy-file is fickle"
  (message (concat "downloading " file))
  (shell-command (concat "wget -c " file " -O " dest " -o /dev/null"))
  (message "done."))

(defun my/untar (file dest)
  "untar file"
  (shell-command (concat "tar xf " file " -C " dest " --strip-components=1")))

(defun my/mkdir-p (dir)
  "make a directory with mkdir -p"
  (shell-command (concat "mkdir -p " dir)))

(defun my/download-modules ()
  "download modules"
  (my/wget "https://github.com/emacs-php/php-mode/archive/v1.21.1.tar.gz" "~/.cache/php-mode.tar.gz")
  (my/wget "https://github.com/leathekd/erc-hl-nicks/archive/1.3.3.tar.gz" "~/.cache/erc-hl-nicks.tar.gz")
  (my/wget "https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/archive/2.0.9.tar.gz" "~/.cache/d-mode.tar.gz"))

;; FIXME:: make this lispy.
(defun my/install-module (file name)
  "install the module"
  ;; make directories
  (my/mkdir-p "~/.cache/my-emacs-tmp/")
  (my/mkdir-p (concat "~/.emacs.d/elisp/" name))
  ;; untar the module
  (my/untar file "~/.cache/my-emacs-tmp/")
  ;; copy .el files from temp folder to NAME
  (shell-command (concat "cp ~/.cache/my-emacs-tmp/*.el ~/.emacs.d/elisp/" name))
  ;; delete the temp folder
  (shell-command "rm -rf ~/.cache/my-emacs-tmp/")
  (message (concat "installed " name)))

(defun my/setup-modules ()
  (interactive)
  (my/download-modules)
  (if (string-equal "87182af418d96c131c39ac8101f144875efafe2229a3523224ff04f77b20cfb8be8ea87365082019ea4679dd1d774b610a22fdcc11ad90176a19f552415299c7"
                    (my/get-sha512sum "~/.cache/php-mode.tar.gz"))
      (my/install-module "~/.cache/php-mode.tar.gz" "php-mode")
    (message "~/.cache/php-mode.tar.gz does not match checksum"))
  (if (string-equal "ae35f138f850e7532c0e2a9a5bbfbe1d61915794f4892d528e3932e745692107272cc78bad632eca6fc798edb4514683f76c19e4da66f3bc19186f108f400d24"
                    (my/get-sha512sum "~/.cache/erc-hl-nicks.tar.gz"))
      (my/install-module "~/.cache/erc-hl-nicks.tar.gz" "erc-hl-nicks")
    (message "~/.cache/erc-hl-nicks.tar.gz does not match checksum"))
  (if (string-equal "753e42ad3b973ce028429e30c9be145989c0aa613f4b89b336d2b69240c58712199986cf9c26e367675d6fcf6ba94ce70db1c67150449e0f70cc9bf67bf141da"
                    (my/get-sha512sum "~/.cache/d-mode.tar.gz"))
      (my/install-module "~/.cache/d-mode.tar.gz" "d-mode")
    (message "~/.cache/d-mode.tar.gz does not match checksum")))


(defun my/compile-the-lot ()
  "compile everything in ~/.emacs.d/elisp regardless of time etc"
  (interactive)
  ;; manually add php-mode to load path first
  (setq load-path (cons "~/.emacs.d/elisp/php-mode" load-path))
  ;; compile
  (byte-recompile-directory "~/.emacs.d/elisp/" 0 t))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(line-number-current-line ((t (:background "color-237" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1")))))
