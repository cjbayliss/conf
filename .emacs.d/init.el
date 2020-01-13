;; setup the package system (comment out after first run for a faster startup)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; don't comment this out otherwise use-package does not work
(package-initialize)

;; this can be commented out after the first run
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(save-place-mode +1)
(global-hl-line-mode +1)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-cl" 'display-line-numbers-mode)

;; custom irc func to load erc and join networks automatcially
(defun irc ()
  "Connect to IRC."
  (interactive)

  ;; these bits need to be here **before** you start ERC
  (setq erc-prompt-for-nickserv-password nil
        ;; set this here, the auto resize is below
        erc-fill-column 157)

  (load "~/.emacs.d/erc")
  (require 'erc-services)
  (erc-services-mode +1)

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
        '(("freenode.net" "#xebian" "#emacs" "#gentoo" "#gentoo-au" "#python" "#allocpsa" "#stumpwm")
          ("oftc.net" "#debian-devel" "#debian-next" "#debian-mentors" "#debian-au" "#packaging"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)
  (global-hl-line-mode -1)
  ;; load erc-hl-nicks
  (use-package erc-hl-nicks :ensure t :defer t)
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

;; hl-todo, making FIXMEs obvious since 2013 apparently
(use-package hl-todo
  :ensure t
  :defer t)

;; don't forget your suit
(use-package company
  :ensure t
  :defer t)

;; all welcome elpy, the fear of pythons
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; yeah, i just using boring old python for this
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"))

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; load php stuff grumble grumble
(use-package php-mode
  :ensure t
  :defer t
  :mode (("\\.php\\'" . php-mode))
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               ;; php-mode settings:
               (setq c-basic-offset 4)
               (php-enable-psr2-coding-style))))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(line-number-current-line ((t (:background "color-237" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1")))))
