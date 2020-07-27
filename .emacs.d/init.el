;; there may be strange approaches in this conifg, here are two reasons why:
;;
;;     1. i'm an idiot who doesn't know elisp.
;;
;;     2. it's faster, and keeps my emacs-init-time below 0.2s on my very old
;;     Macbook. (current init time 2019-04-27: 0.0s)
;;
;; NOTE: a better way to test the startup time is this 'oneliner' (the sleep is important):
;;
;;     $ x=0; while [ $x -lt 10 ]; do time emacs --no-site-lisp -nw -kill; x=$((x + 1)); sleep 2; done
;;
;; on my Macbook 4,1 (2008) the average is: 62.4ms

;; general emacs settings
(setq inhibit-startup-screen t
      column-number-mode t
      make-backup-files nil
      require-final-newline t
      c-basic-offset 4
      browse-url-browser-function 'eww-browse-url

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

      ;; get stuff out of the home dir
      gnus-directory (concat user-emacs-directory "news")
      gnus-startup-file (concat user-emacs-directory "newsrc")
      gnus-init-file (concat user-emacs-directory "gnus")
      ;; setting this to low has an impact on startup, so set it high, then set
      ;; it low later in emacs-start-hook
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      package--init-file-ensured t
      initial-scratch-message ";; ┌─┐┌┐┬┬ ┬ ┌─┐┌┬┐┌─┐┌─┐┌─┐
;; │┌┐│└┤│ │ ├┤ │││├─┤│  └─┐
;; └─┘┴ ┴└─┘ └─┘┴ ┴┴ ┴└─┘└─┘
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

")

(setq-default fill-column 79
              frame-background-mode 'dark
              indent-tabs-mode nil
              show-trailing-whitespace t)

;; enable/disable modes
(show-paren-mode +1)
(delete-selection-mode +1)
(menu-bar-mode -1)

;; yes, these modes slow down startup by about 4ms total 😢
(save-place-mode +1)
(global-hl-line-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; keybinds
(global-set-key "\C-cb" 'browse-url-at-point)
(global-set-key "\C-cl" 'display-line-numbers-mode)
(global-set-key "\C-ch" 'hl-line-mode)
(global-set-key "\C-cf" 'my/neotree-toggle)

;; FIXME: switch to SASL. I tried circe, but almost pulled my hair out.
;; custom irc func to load erc and join networks automatically
(defun irc ()
  "Connect to IRC."
  (interactive)

  ;; these bits need to be here **before** you start ERC
  (setq erc-prompt-for-nickserv-password nil
        ;; set this here, the auto resize is below
        erc-fill-column 157)

  (load "~/.emacs.d/erc")
  (require 'erc-services)
  (erc-services-mode +1)

  (erc-tls :server "chat.au.freenode.net" :port 6697 :nick "cjb" :full-name "Christopher Bayliss")
  (erc-tls :server "irc.oftc.net" :port 6697 :nick "cjbayliss" :full-name "Christopher Bayliss"))

;; ERC config
(with-eval-after-load "erc"
  (require 'erc-goodies)

  (setq erc-autojoin-channels-alist
        '(("freenode.net" "##asm" "#emacs" "#freenode" "#guile" "#guix" "#lisp" "#scheme" "#xebian")
          ("oftc.net" "#debian-devel" "#debian-next"))
        erc-autojoin-timing 'ident
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT")
        erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-prompt-for-nickserv-password nil
        erc-prompt-for-password nil
        erc-rename-buffers t
        erc-server "chat.au.freenode.net"
        erc-server-reconnect-timeout 60
        erc-track-exclude-server-buffer t
        erc-user-full-name "Christopher Bayliss")

  (erc-notifications-mode +1)
  (erc-scrolltobottom-enable)
  (erc-spelling-mode +1)
  (ido-mode +1)
  (global-hl-line-mode -1)
  (setq-default show-trailing-whitespace nil)
  (show-paren-mode -1)

  ;; load and require erc-hl-nicks
  (my/load-lisp)
  (require 'erc-hl-nicks)
  (erc-hl-nicks)

  ;; make ERC use full buffer width
  (add-to-list 'window-configuration-change-hook
               (lambda () (setq erc-fill-column (- (window-width) 2))))
  ;; keep ERC buffer pined to bottom
  (add-to-list 'erc-mode-hook
               (lambda () (set (make-local-variable 'scroll-conservatively) 100)))

  ;; fix ERC prompt colours
  (custom-set-faces '(erc-prompt-face ((t (:foreground "brightwhite" :background nil :weight bold))))
                    '(erc-input-face ((t (:foreground "white")))))

  ;; BEHOLD!! this lone paren, isn't it beautiful? One must wonder what life it
  ;; has lived, but since you know how to use git you'll find out in no time!!
  ;; (yes, I felt like writing about this paren for no reason at all.)
  )

;; load php stuff grumble grumble
(add-to-list 'auto-mode-alist
             '("\\.php\\'" . (lambda ()
                               (my/load-lisp)
                               (require 'php-mode)
                               (php-mode)
                               (setq c-basic-offset 4)
                               (php-enable-psr2-coding-style))))

;; load nasm-mode instead of the broken(?) asm-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(asm\\|nasm\\|S\\|s\\)\\'" . (lambda ()
                                                    (my/load-lisp)
                                                    (require 'nasm-mode)
                                                    (nasm-mode))))

;; lua-mode
(add-to-list 'auto-mode-alist
             '("\\.lua\\'" . (lambda ()
                               (my/load-lisp)
                               (require 'lua-mode)
                               (lua-mode))))

;; if I'm editing a C file, I *probably* want the linux style
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux"
                  c-basic-offset 8
                  tab-width 8
                  indent-tabs-mode t)))

;; instead of loading hl-todo (which compiled, takes about 10ms on my machine)
(defface highlight-todo-face
  '((t :foreground "yellow" :background "black" :inverse-video t :weight bold))
  "Basic face for highlighting TODO &c.")
(defvar highlight-todo-face 'highlight-todo-face)
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 highlight-todo-face t)))))

;; emacs should already have this function, does it and i can't find it?
(defun byte-compile-directory (directory)
  "compile contents of a directory"
  (interactive "DByte compile directory: ")
  (byte-recompile-directory directory 0))

;; it takes almost 10ms add these load paths, so put it in a function, and call
;; when needed instead of every time i open emacs
(defun my/load-lisp ()
  "function to load lisp when needed"
  ;; add site-lisp and user lisp to load path
  (let ((default-directory  "/usr/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (let ((default-directory
          (concat user-emacs-directory "lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

;; my prefered packages
(defvar my/packages
  '(elfeed erc-hl-nicks lua-mode nasm-mode neotree php-mode which-key))

;; custom install function
(defun my/install-packages ()
  "install my/packages"
  (interactive)
  (require 'package)
  (my/load-lisp)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; create and set lisp directory
  (mkdir (concat user-emacs-directory "lisp") t)
  (setq package-user-dir (concat user-emacs-directory "lisp"))
  (package-initialize)
  (package-refresh-contents)
  (dolist (x my/packages)
    (unless (package-installed-p x)
      (package-install x))))

;; elfeed setup
(defun rss()
  "function to load then start elfeed"
  (interactive)
  (my/load-lisp)
  (setq-default show-trailing-whitespace nil)

  ;; if we don't create ~/.emacs.d/elfeed, elfeed will create ~/.elfeed instead
  ;; of creating elfeed-db-directory
  (mkdir (concat user-emacs-directory "elfeed") t)

  ;; set elfeed's... feeds
  (setq elfeed-feeds '("https://blog.jeff.over.bz/rss.xml"
                       "https://blog.mattcen.com/rss"
                       "https://christine.website/blog.rss"
                       "https://cjb.sh/articles/feed.xml"
                       "https://codingquark.com/feed.xml"
                       "https://danluu.com/atom.xml"
                       "https://deftly.net/rss.xml"
                       "https://jvns.ca/atom.xml"
                       "https://microkerneldude.wordpress.com/feed/"
                       "https://nullprogram.com/feed/"
                       "https://pine64.org/blog/rss"
                       "https://planet.freedesktop.org/rss20.xml"
                       "https://planet.gentoo.org/rss20.xml"
                       "https://planet.gnu.org/atom.xml"
                       "https://planet.kernel.org/rss20.xml"
                       "https://rachelbythebay.com/w/atom.xml"
                       "https://sachachua.com/blog/category/emacs-news/feed"
                       "https://security.gentoo.org/glsa/feed.rss"
                       "https://voicesoftheelephpant.com/feed/podcast/")
        elfeed-db-directory (concat user-emacs-directory "elfeed"))
  (require 'elfeed)
  (elfeed))

;; load neotree if not loaded and toggle
(defun my/neotree-toggle()
  (interactive)
  (unless (featurep 'neotree)
    (my/load-lisp)
    (require 'neotree))
  (neotree-toggle))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; a high gc-cons-threshold causes input lag on my laptop every x
            ;; seconds, so set it crazy small 😬
            (setq gc-cons-threshold 100
                  gc-cons-percentage 0.1)
            ;; load which-key with little startup impact
            (add-to-list 'load-path (car (file-expand-wildcards (concat user-emacs-directory "lisp/which-key*") t)))
            (require 'which-key)
            (which-key-mode)))

;; believe it or not, this **doesn't** increase emacs init time
(custom-set-faces
 '(line-number-current-line ((t (:background "darkolivegreen" :foreground "chocolate1"))))
 '(mode-line-buffer-id ((t (:foreground "red" :background nil :weight bold :slant oblique))))
 '(region ((t (:inverse-video t))))
 '(show-paren-match ((t (:foreground "steelblue1"))))
 '(vc-edited-state ((t (:foreground "#553333" :slant oblique :weight bold))))
 '(vc-up-to-date-state ((t (:foreground "#335533" :slant oblique :weight bold)))))

;; ah shit, you've mistakenly opened graphical emacs, here let me help you out:
(when (display-graphic-p)
  (set-background-color "black")
  (set-foreground-color "white")
  (custom-set-faces '(mode-line ((t (:background "grey75" :foreground "black")))))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (when (member "Iosevka Term Slab" (font-family-list))
    (set-frame-font "Iosevka Term Slab-11" t t))
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))
  ;; why this **isn't** default is beyond me.
  (setq mouse-yank-at-point t)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
