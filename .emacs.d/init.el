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

;; ERC config
(with-eval-after-load "erc"
  (autoload 'erc-goodies "erc-goodies")

  (defun my/return-lines (file)
    "return list of lines from file"
    (with-temp-buffer (insert-file-contents file)
                      (split-string (buffer-string) "\n" t)))

  (let ((acc (my/return-lines "~/.my-erc-account")))
    (setq erc-nick (car acc)
          erc-password (nth 1 acc)))
  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-server "chat.au.freenode.net"
        erc-autojoin-channels-alist
        '(("freenode.net" "#xebian")("oftc.net" "#debian-devel"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (erc-scrolltobottom-enable)
  (erc-notifications-mode)
  (erc-spelling-mode)
  (global-display-line-numbers-mode 0)
  (global-hl-line-mode 0)

  ;; make ERC use full buffer width
  (add-to-list 'window-configuration-change-hook
               (lambda () (setq erc-fill-column (- (window-width) 2))))
  ;; keep ERC buffer pined to bottom
  (add-to-list 'erc-mode-hook
               (lambda () (set (make-local-variable 'scroll-conservatively) 100)))

  ;; fix ERC prompt colors
  (custom-set-faces '(erc-prompt-face ((t (:foreground "brightwhite" :background nil :weight bold))))
                    '(erc-timestamp-face ((t (:foreground "brightwhite" :weight bold))))
                    '(erc-input-face ((t (:foreground "white"))))
                    '(erc-my-nick-face ((t (:foreground "brightred" :weight bold)))))

  ;; modified options two and five from here:
  ;; https://www.emacswiki.org/emacs/ErcNickColors

  ;; create a list of colors without colors too dark to see
  (setq my-colors-list '(nil))
  (dolist (color (defined-colors))
    (or (string-match-p "black" color)
        (string-match-p "color-16" color)
        (string-match-p "color-17" color)
        (string-match-p "color-18" color)
        (string-match-p "color-22" color)
        (string-match-p "color-52" color)
        (string-match-p "color-53" color)
        (string-match-p "color-54" color)
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
                           ("parsnip" . "color-57")
                           ("bremner" . "color-226")
                           ("bpalmer". "color-241")
                           ("jlf" . "color-191")
                           ("cjb" . "color-85")))

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
 '(mode-line ((t (:foreground "white" :slant oblique))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold))))
 '(mode-line-inactive ((t (:foreground "white"))))
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
 '(region ((t (:inverse-video t))))
 '(rst-level-1 ((t (:weight bold))))
 '(rst-level-2 ((t (:weight bold))))
 '(rst-level-3 ((t (:weight bold))))
 '(rst-level-4 ((t (:weight bold))))
 '(rst-level-5 ((t (:weight bold))))
 '(trailing-whitespace ((t (:background "red"))))
 '(widget-field ((t (:background "white" :foreground "black")))))
