;; there may be strange approaches in this conifg, here are two reasons why:
;;
;;     1. i'm an idiot who doesn't know elisp.
;;
;;     2. it's faster, and keeps my emacs-init-time below 0.2s on my 11 year
;;     old Macbook. (current init time 2019-04-27: 0.0s)
;;
;; I install packages using the gs-elpa overlay, thus removing the startup
;; delay caused by installing elpa/melpa packages to ~/.emacs.d/elpa/

;; general emacs settings
(setq inhibit-startup-screen t
      column-number-mode t
      make-backup-files nil
      require-final-newline t
      ;; w3m config -- somewhat copied twb's defaults
      w3m-bookmark-file-coding-system 'utf-8
      w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-default-save-directory "~/downloads/"
      ;; this is such a horrible default, disable it
      w3m-enable-google-feeling-lucky nil
      w3m-pop-up-windows nil
      ;; use the User-Agent from ~/.w3m/config
      w3m-add-user-agent nil
      w3m-search-engine-alist
      '(("wikipedia" "https://en.wikipedia.org/wiki/Special:Search/%s")
        ("duckduckgo" "https://duckduckgo.com/lite/?q=%s"))
      w3m-search-default-engine "duckduckgo"
      ;; http://bugs.debian.org/457909
      w3m-show-decoded-url nil
      w3m-track-mouse nil
      w3m-use-cookies nil
      w3m-use-favicon nil
      w3m-use-header-line nil
      w3m-use-mule-ucs t
      ;; always use Unicode (not ASCII) box drawing characters.
      w3m-use-symbol t
      w3m-use-tab-menubar nil
      w3m-use-toolbar nil
      w3m-home-page "about:blank"
      browse-url-browser-function 'w3m-browse-url
      ;; don't show gnus startup, and use mutt like threading. copied from:
      ;; http://cyber.com.au/~twb/.emacs
      gnus-inhibit-startup-message t
      gnus-sum-thread-tree-false-root "─*> "
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-leaf-with-other "├─> "
      gnus-sum-thread-tree-root "> "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-single-leaf "└─> "
      gnus-sum-thread-tree-vertical "│ "
      gnus-user-date-format-alist '((t . "%b %e"))
      gnus-summary-line-format "%4N %U%R%z %&user-date; %-14,14n (%4k) %B%s\n"
      ;; set this very low to reduce input lag, this can slow down init
      ;; time. people in the know say this shouldn't be needed. idk, i simply
      ;; want to type without a lag spike every 15 words or so. maybe it's
      ;; because my ram is slow and old?
      gc-cons-threshold 100
      initial-scratch-message ";; ┏━╸┏┓╻╻ ╻ ╔═╗┌┬┐┌─┐┌─┐┌─┐
;; ┃╺┓┃┗┫┃ ┃ ║╣ │││├─┤│  └─┐
;; ┗━┛╹ ╹┗━┛ ╚═╝┴ ┴┴ ┴└─┘└─┘
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
(global-hl-line-mode +1)
(global-display-line-numbers-mode +1)
;; disable some modes
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)

;; disable some global stuff in w3m
(with-eval-after-load "w3m"
  (setq show-trailing-whitespace nil)
  (global-hl-line-mode -1)
  (global-display-line-numbers-mode -1))

;; custom irc func to load erc and join networks automatcially
(defun my/irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "chat.au.freenode.net" :port 6697 :nick "cjb" :full-name "Christopher Bayliss")
  (erc-tls :server "irc.oftc.net" :port 6697 :nick "cjbayliss" :full-name "Christopher Bayliss"))

;; ERC config
(with-eval-after-load "erc"
  (autoload 'erc-goodies "erc-goodies")

  (setq erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-autojoin-timing 'ident
        erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-autojoin-channels-alist
        '(("freenode.net" "#xebian" "#allocpsa")
          ("oftc.net" "#debian-devel"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)
  (global-hl-line-mode -1)
  (global-display-line-numbers-mode -1)
  (erc-hl-nicks)
  (erc-scrolltobottom-enable)
  (erc-notifications-mode +1)
  (erc-spelling-mode +1)
  (erc-services-mode +1)

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
(add-to-list 'auto-mode-alist '("\\.\\(?:php\\|phtml\\)\\'" . php-mode))
(with-eval-after-load "php-mode"
  (setq c-basic-offset 4))
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;; js2-mode config
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

;; add a hook to highlight the current day in the calendar
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(calendar-today ((t (:foreground "red" :weight bold :slant oblique))))
 '(hl-line ((t (:background "color-237"))))
 '(js2-function-param ((t (:foreground "white" :slant oblique))))
 '(js2-object-property-access ((t (:foreground "color-115"))))
 '(line-number-current-line ((t (:background "color-237" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1")))))
