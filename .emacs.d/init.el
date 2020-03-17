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

;; un-comment this to install stuff from the melpa repo. make sure to comment it
;; out for better startup times

;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; fix ugly colours
(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "red" :background nil  :weight bold)))))
