;; general emacs settings
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      column-number-mode t
      make-backup-files nil
      ;; set this very low to reduce input lag
      gc-cons-threshold 100
      ;; use the one and only C style!
      c-default-style "linux")

;; set the default fill-column char width
(setq-default fill-column 80)

;; enable line numbers & highlight line
(global-display-line-numbers-mode)
(global-hl-line-mode)

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(custom-state ((t (:foreground "brightgreen"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "brightyellow"))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-constant-face ((t (:foreground "brightmagenta"))))
 '(font-lock-function-name-face ((t (:foreground "brightblue"))))
 '(font-lock-keyword-face ((t (:foreground "brightred"))))
 '(font-lock-preprocessor-face ((t (:foreground "brightcyan"))))
 '(font-lock-string-face ((t (:foreground "brightgreen"))))
 '(font-lock-type-face ((t (:foreground "brightyellow"))))
 '(font-lock-variable-name-face ((t (:foreground "brightred"))))
 '(header-line ((t (:foreground "brightwhite" :slant oblique :weight bold))))
 '(hl-line ((t (:background "color-237"))))
 '(isearch ((t (:background "brightyellow" :foreground "black"))))
 '(lazy-highlight ((t (:background "red" :foreground "black"))))
 '(line-number-current-line ((t (:background "color-237" :foreground "yellow"))))
 '(link ((t (:foreground "color-117" :underline t))))
 '(minibuffer-prompt ((t (:foreground "brightwhite" :weight bold))))
 '(mode-line ((t (:foreground "white" :slant oblique))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold))))
 '(mode-line-inactive ((t (:foreground "white"))))
 '(region ((t (:inverse-video t))))
 '(rst-level-1 ((t (:weight bold))))
 '(rst-level-2 ((t (:weight bold))))
 '(rst-level-3 ((t (:weight bold))))
 '(rst-level-4 ((t (:weight bold))))
 '(rst-level-5 ((t (:weight bold))))
 '(trailing-whitespace ((t (:background "red"))))
 '(widget-field ((t (:background "white" :foreground "black")))))
