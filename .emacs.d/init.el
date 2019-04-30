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

  ;; modified options two and five from here:
  ;; https://www.emacswiki.org/emacs/ErcNickColors

  ;; create a list of colors without colors too dark to see
  (setq my-colors-list '(nil))
  (dolist (color (defined-colors))
    (or (string-match-p "black" color)
        (string-match-p "color-16" color)
        (string-match-p "color-17" color)
        (string-match-p "color-18" color)
        (string-match-p "color-19" color)
        (string-match-p "color-20" color)
        (string-match-p "color-21" color)
        (string-match-p "color-22" color)
        (string-match-p "color-52" color)
        (string-match-p "color-53" color)
        (string-match-p "color-54" color)
        (string-match-p "color-55" color)
        (string-match-p "color-56" color)
        (string-match-p "color-57" color)
        (string-match-p "color-232" color)
        (string-match-p "color-233" color)
        (string-match-p "color-234" color)
        (string-match-p "color-235" color)
        (string-match-p "color-236" color)
        (string-match-p "color-237" color)
        (add-to-list 'my-colors-list color)))

  ;; people i associate their with a color from previous nick color scripts
  (setq my-custom-colors '(("Unit193" . "color-92")
                           ("twb" . "color-226")
                           ("codingquark" . "color-157")
                           ("parsnip" . "color-69")
                           ("parsnip0" . "color-69")
                           ("bremner" . "color-226")
                           ("bpalmer". "color-241")
                           ("jlf" . "color-191")
                           ("fsbot" . "color-219")
                           ("cjb" . "color-66")))

  (defun my/return-color (string)
    "return color for STRING"
    (or (cdr (assoc nick my-custom-colors))
        (nth (mod (string-to-number (substring (md5 (downcase string)) 0 6) 16)
                  (length my-colors-list))
             my-colors-list)))

  (defun erc-highlight-nicknames ()
    "highlight erc nicknames with color from my/return-color"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\w+" nil t)
        (let* ((bounds (bounds-of-thing-at-point 'symbol))
               (nick   (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (when (erc-get-server-user nick)
            (put-text-property
             (car bounds) (cdr bounds) 'face
             (cons 'foreground-color (my/return-color nick)))
            (add-face-text-property
             (car bounds) (cdr bounds) 'bold))))))

  (add-hook 'erc-insert-modify-hook 'erc-highlight-nicknames)
  (add-hook 'erc-send-modify-hook 'erc-highlight-nicknames))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t)))))
