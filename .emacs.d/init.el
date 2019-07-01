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

  ;; this highlighting is a slightly modified version of option five from here:
  ;; https://www.emacswiki.org/emacs/ErcNickColors
  (defmacro unpack-color (color red green blue &rest body)
    `(let ((,red   (car ,color))
           (,green (car (cdr ,color)))
           (,blue  (car (cdr (cdr ,color)))))
       ,@body))

  (defun rgb-to-html (color)
    (unpack-color color red green blue
                  (concat "#" (format "%02x%02x%02x" red green blue))))

  (defun hexcolor-luminance (color)
    (unpack-color color red green blue
                  (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))

  (defun invert-color (color)
    (unpack-color color red green blue
                  `(,(- 255 red) ,(- 255 green) ,(- 255 blue))))

  (defun erc-get-color-for-nick (nick dark)
    (let* ((hash     (md5 (downcase nick)))
           (red      (mod (string-to-number (substring hash 0 10) 16) 256))
           (blue     (mod (string-to-number (substring hash 10 20) 16) 256))
           (green    (mod (string-to-number (substring hash 20 30) 16) 256))
           (color    `(,red ,green ,blue)))
      (rgb-to-html (if (if dark (< (hexcolor-luminance color) 85)
                         (> (hexcolor-luminance color) 170))
                       (invert-color color)
                     color))))

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
             (cons 'foreground-color (erc-get-color-for-nick nick 'nil)))
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
