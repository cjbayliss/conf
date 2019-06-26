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
              frame-background-mode 'light
              indent-tabs-mode nil
              show-trailing-whitespace t)

;; add debian's elpa packges to load path
(let ((default-directory  "/usr/share/emacs/site-lisp/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

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
  ;; these bits need to be here **before** you start ERC
  (setq erc-prompt-for-nickserv-password nil
        ;; set this here, the auto resize is below
        erc-fill-column 157)
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
        '(("freenode.net" "#xebian" "#emacs" "#allocpsa" "#stumpwm")
          ("oftc.net" "#debian-devel" "#debian-next" "#debian-mentors"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)
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
  (custom-set-faces '(erc-prompt-face ((t (:foreground "black" :background nil :weight bold)))))

  ;; modified options two and five from here:
  ;; https://www.emacswiki.org/emacs/ErcNickColors

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FIXME:: package erc-hl-nicks for debian to get rid of this mess ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                           ("cjb" . "color-66")
                           ("cjbayliss" . "color-66")))

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
  (add-hook 'erc-send-modify-hook 'erc-highlight-nicknames)

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what life it
  ;; has lived, but since you know how to use git you'll find out in no time!!
  ;; (yes, I felt like writing about this paren for no reason at all.)
  )

;; php-mode config
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (autoload 'php-mode "php-mode")
                               (php-mode)
                               (setq c-basic-offset 4)
                               (php-enable-psr2-coding-style))))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "VioletRed4" :background nil :weight bold :slant oblique)))))
