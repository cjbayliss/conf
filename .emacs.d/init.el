;; there may be strange approaches in this conifg, here are two reasons why:
;;
;;     1. i'm an idiot who doesn't know elisp.
;;
;;     2. it's faster, and keeps my emacs-init-time below 0.2s on my 11 year
;;     old Macbook. (current init time 2019-04-27: 0.0s)

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
              frame-background-mode 'dark
              indent-tabs-mode nil
              show-trailing-whitespace t)


;; enable/disable modes
(show-paren-mode +1)
(delete-selection-mode +1)
(save-place-mode +1)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-cl" 'display-line-numbers-mode)

;; rcirc enable spell checking, rcirc-omit-mode, and pin prompt to bottom
(add-hook 'rcirc-mode-hook
          (lambda ()
            (flyspell-mode +1)
            (rcirc-omit-mode +1)
            (rcirc-track-minor-mode +1)
            (set (make-local-variable 'scroll-conservatively) 8192)))
;; make rcirc use full window width
(add-to-list 'window-configuration-change-hook
             (lambda () (setq rcirc-fill-column (- (window-width) 2))))

(setq rcirc-default-nick "cjb"
      rcirc-default-user-name "cjb"
      rcirc-default-full-name "Christopher Bayliss"
      rcirc-startup-channels-alist nil
      rcirc-buffer-maximum-lines 2048
      rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
      rcirc-server-alist '(("chat.au.freenode.net" :nick "cjb" :port 6697 :encryption tls
                            :channels ("#xebian" "#emacs" "#allocpsa" "#stumpwm"))
                           ("127.0.0.1" :nick "cjb" :port 6667 :channels ("#cyber"))
                           ("irc.oftc.net" :nick "cjbayliss" :port 6697 :encryption tls
                            :channels ("#debian-devel" "#debian-next" "#debian-mentors"))))

(with-eval-after-load "rcirc"
  ;; disable thing unrelated to IRC
  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)

  ;; notifications
  (require 'notifications)
  (defun rcirc-notifications (process sender response target text)
    (when (and (string= response "PRIVMSG")
               (not (string= sender (rcirc-nick process)))
               (not (rcirc-channel-p target)))
      (notifications-notify :title sender :body text))
    (when (and (string-match (rcirc-nick process) text)
               (rcirc-channel-p target)
               (not (string= (rcirc-nick process) sender))
               (not (string= (rcirc-server-name process) sender)))
      (notifications-notify :title sender :body text)))

  (add-hook 'rcirc-print-hooks 'rcirc-notifications))

(load "~/.rcirc" t t)

;; add debian's elpa packges to load path
(let ((default-directory  "/usr/share/emacs/site-lisp/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; load php stuff grumble grumble
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (autoload 'php-mode "php-mode")
                               (php-mode)
                               (setq c-basic-offset 4)
                               (php-enable-psr2-coding-style))))

;; beleive it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(line-number-current-line ((t (:background "color-237" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1")))))
