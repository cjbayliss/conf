;; there may be strange approaches in this conifg, here are two reasons why:
;;
;;     1. i'm an idiot who doesn't know elisp.
;;
;;     2. it's faster, and keeps my emacs-init-time below 0.2s on my 11 year
;;     old Macbook. (current init time 2019-04-27: 0.0s)
;;
;; general emacs settings
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      column-number-mode t
      make-backup-files nil
      ;; set this very low to reduce input lag, this can slow down init time
      gc-cons-threshold 100
      ;; use the one and only C style!
      c-default-style "linux")

;; set the default fill-column char width
(setq-default fill-column 79)

;; enable line numbers & highlight line
(global-display-line-numbers-mode)
(global-hl-line-mode)

;; set the mode-line colours here to prevent a flash/flicker
(custom-set-faces
 '(mode-line ((t (:foreground "white" :slant oblique))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold))))
 '(mode-line-inactive ((t (:foreground "white")))))

;; load additional colours (separate file to keep this init.el pretty)
(load-file "~/.emacs.d/colours.el")
