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
      ;; set this very low to reduce input lag, this can slow down init
      ;; time. people in the know say this shouldn't be needed. idk, i simply
      ;; want to type without a lag spike every 15 words or so. maybe it's
      ;; because my ram is slow and old?
      gc-cons-threshold 100)

;; set the default fill-column char width
(setq-default fill-column 79
              frame-background-mode 'dark
              indent-tabs-mode nil)

;; ERC config
(with-eval-after-load "erc"
  (autoload 'erc-goodies "erc-goodies")

  (setq erc-prompt-for-password nil
        erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-server "chat.au.freenode.net"
        erc-nick "cjb"
        erc-user-full-name "Christopher Bayliss"
        erc-autojoin-channels-alist
        '(("freenode.net" "#xebian" "#emacs" "#allocpsa" "#stumpwm" "#gnu" "#guile")
          ("oftc.net" "#debian-devel"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  ;; load erc-hl-nicks
  (load "~/.emacs.d/elisp/erc-hl-nicks")
  (erc-hl-nicks)
  (erc-scrolltobottom-enable)
  (erc-notifications-mode)
  (erc-spelling-mode)

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

;; finally, i can use php-mode and phpcbf without slowing down the rest of my
;; life
(add-to-list 'auto-mode-alist
             '("\\.php\\'" .
               (lambda ()
                 (load "~/.emacs.d/elisp/php-project")
                 (load "~/.emacs.d/elisp/php-mode")
                 (php-mode)
                 (setq indent-tabs-mode nil
                       c-basic-offset 4)
                 (php-enable-psr2-coding-style))))

(defun my/compile-the-lot ()
  "compile everything in ~/.emacs.d/elisp regardless of time etc"
  (interactive)
  ;; add ~/.emacs/elisp to the load path so that everything compiles,
  ;; e.g. php-mode.el depends on php-project.el
  (setq load-path (cons "~/.emacs.d/elisp/" load-path))
  ;; compile
  (byte-recompile-directory "~/.emacs.d/elisp/" 0 t))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t)))))
