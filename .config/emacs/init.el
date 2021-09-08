;;; Init --- GNU Emacs initialisation file  -*- lexical-binding: t -*-

;; Author: Christopher Bayliss <cjb@cjb.sh>
;; Created: 2021-05-05
;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:

;; Complete rewrite of my old Emacs initialisation file

;; Raison d'être: readability > correctness > speed

;;; Code:
;; begin init
(setq gc-cons-threshold most-positive-fixnum)

;;; General:
;;;; ensure-pkg
;; IMPORTANT: this MUST be defined before it's called later in the init
;; file. (where you define an elisp function doesn't normally matter,
;; but does in this case. WHY??)
(defun ensure-pkg (pkg)
  "Ensure PKG is installed."
  (require 'package)
  (unless (package-installed-p pkg)
    (package-initialize)
    (package-refresh-contents)
    (package-install pkg)))

;;;; sane defaults
(setq auth-source-save-behavior nil)
(setq browse-url-handlers '(("." . browse-url-firefox)))
(setq c-basic-offset 4)
(setq column-number-mode t)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq help-window-select t)
(setq make-backup-files nil)
(setq org-cycle-include-plain-lists 'integrate)
(setq package--init-file-ensured t)
(setq require-final-newline t)
(setq shell-file-name "sh")
(setq split-width-threshold 158)
(setq tab-always-indent 'complete)
(setq tramp-allow-unsafe-temporary-files t)
(setq-default fill-column 72)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)

;;;; misc options/changes
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)

;; upcase-region & downcase-region are disabled, but not this. WTF??
(fmakunbound 'overwrite-mode)

;; don't popup the async-shell-command buffer
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*"
                   (cons #'display-buffer-no-window nil)))

;;;; modus themes
(ensure-pkg 'modus-themes)
(setq modus-themes-slanted-constructs t)
(setq modus-themes-no-mixed-fonts t)
(load-theme 'modus-vivendi t)

;;;; disable/enable various global modes
(menu-bar-mode -1)
(save-place-mode +1)

;; these modes are slow to load, add them to this hook instead
(add-hook 'emacs-startup-hook
          (lambda ()
            (ensure-pkg 'marginalia)
            (marginalia-mode +1)
            (delete-selection-mode +1)
            (savehist-mode +1)
            (show-paren-mode +1)))

;; global-hl-line-mode is overkill
(mapc (lambda (x)
        (add-hook x 'hl-line-mode +1))
      '(dired-mode-hook
        text-mode-hook))

;;;; disable/change startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message (concat "In the beginning the Emacs was created. This has "
                   "made a lot of people very angry and been widely "
                   "regarded as a bad move.")))

;;;; setup fonts
(when (display-graphic-p)
  ;; default font
  (when (member "Iosevka Fixed" (font-family-list))
    (set-frame-font "Iosevka Fixed-11" 'keep-size t))
  ;; 😜
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))
  ;; 한국어/조선말
  (when (member "Baekmuk Gulim" (font-family-list))
    (set-fontset-font t 'unicode "Baekmuk Gulim-11" nil 'prepend))
  ;; 日本語
  (when (member "IPAGothic" (font-family-list))
    (set-fontset-font t 'unicode "IPAGothic-11" nil 'prepend))
  ;; Latin/Cyrillic
  (when (member "Iosevka Fixed" (font-family-list))
    (set-fontset-font t 'unicode "Iosevka Fixed-11" nil 'prepend)))

;;;; setup GUI only stuff
(when (display-graphic-p)
  (setq mouse-yank-at-point t)
  (setq x-gtk-use-system-tooltips nil)
  (setq-default cursor-type '(hbar . 2))
  (fringe-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (server-start))

;;;; keybindings
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c m") 'proced)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c p") 'run-python)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 🤬
(global-unset-key (kbd "C-z"))

;;;; configuring the mode-line is pretty ugly 🤮
(add-hook
 'emacs-startup-hook
 (lambda ()
   (delete (nth 4 mode-line-modes) mode-line-modes)
   (setq-default
    mode-line-format
    '("%e"
      mode-line-front-space
      mode-line-mule-info
      mode-line-client
      (:eval (if (buffer-modified-p)
                 (format-mode-line 'mode-line-modified 'warning)
               mode-line-modified))
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      "   "
      mode-line-position
      (vc-mode vc-mode)
      " "
      (:eval (when (boundp 'rcirc-activity)
               (when rcirc-activity
                 rcirc-activity-string)))
      " "
      (:eval (format-mode-line 'mode-line-modes 'font-lock-doc-face))
      (:eval (format-mode-line '(" " display-time-string) 'bold))
      "  "
      (:eval (format-mode-line mode-line-misc-info
                               'font-lock-comment-delimiter-face))
      mode-line-end-spaces))

   (display-time-mode +1)
   (delq 'display-time-string global-mode-string)))

;;; Tools
;;;; dired
(setq dired-listing-switches "-ABlhFv")
(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; quit means quit, please!
            (define-key dired-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))))

;;;; eshell
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 4096)
(setq eshell-input-filter 'eshell-input-filter-initial-space)
(setq eshell-ls-initial-args "-h")
(setq eshell-scroll-to-bottom-on-input 'all)

(add-hook 'eshell-mode-hook
          (lambda ()
            (goto-address-mode +1)
            (setenv "PAGER" "cat")
            ;; these in term
            (add-to-list 'eshell-visual-commands "mpv")
            (add-to-list 'eshell-visual-commands "nix-shell")
            (add-to-list 'eshell-visual-commands "ssh")
            ;; stopping the world to process file operations is insane.
            (fmakunbound 'eshell/cp)
            (fmakunbound 'eshell/mv)
            (fmakunbound 'eshell/rm)
            ;; eshell/date is inferior to GNU Coreutils date(1)
            (fmakunbound 'eshell/date)))

;;;; eww
(setq eww-download-directory (expand-file-name "~/downloads"))
(setq eww-header-line-format nil)
(setq eww-search-prefix "https://duckduckgo.com/lite/?k1=-1&q=")
(setq shr-cookie-policy nil)
(setq shr-discard-aria-hidden t)
(setq shr-max-image-proportion 0.6)
(setq shr-use-colors nil)
(setq shr-use-fonts nil)

;; rename eww buffers after rendering
(add-hook 'eww-after-render-hook
          (lambda ()
            (unless (string-empty-p (plist-get eww-data :title))
              (rename-buffer (plist-get eww-data :title) t))))

;; custom keybindings
(add-hook 'eww-mode-hook
          (lambda ()
            (define-key eww-link-keymap (kbd "RET") 'eww-open-in-new-buffer)
            (define-key eww-mode-map (kbd "q")
              (lambda () (interactive) (quit-window t)))))

;;;; etags
(defun ctags-scan-dir (directory)
  "Generate a TAGS file in DIRECTORY using universal-ctags."
  (call-process-shell-command
   (format "ctags -f %sTAGS -e -R %s" directory directory)))

(defun vc-generate-etags ()
  "Generate a TAGS file in the vc-root-dir."
  (interactive)
  (let ((root (vc-root-dir)))
    (or (when root
          (message (format "Generating TAGS file for %s ..." root))
          (ctags-scan-dir root)
          (message (format "Generating TAGS file for %s ... Done." root)))
        (message "Can't find the version control root directory."))))

(define-key global-map (kbd "C-x v t") 'vc-generate-etags)

;;;; vertico
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; use completing-read for completion in region
(setq completion-in-region-function
      #'completing-read-completion--in-region)

(ensure-pkg 'vertico)
(vertico-mode +1)

;;;; ix.io paste tool
(defun ix-io--process-response (response)
  "Process RESPONSE from ix.io"
  ;; FIXME: lmao, this is totally going to break someday
  (let ((url (nth 9 (string-lines response))))
    (kill-new url)
    (message (concat url " copied to kill ring."))))

(defun ix-io--post (data)
  "Post DATA to ix.io, and copy url response to kill-ring."
  (let ((url-request-method "POST")
        (url-request-data (concat "f:1="
                                  (url-hexify-string data))))
    (with-current-buffer (url-retrieve-synchronously "http://ix.io")
      (ix-io--process-response (buffer-string)))))

(defun ix-io-paste-buffer ()
  "Paste buffer using ix.io"
  (interactive)
  (when (yes-or-no-p "Paste buffer? ")
    (ix-io--post (buffer-substring (point-min) (point-max)))))

(defun ix-io-paste-region (start end)
  "Paste buffer using ix.io"
  (interactive "r")
  (when (yes-or-no-p "Paste region? ")
    (ix-io--post (buffer-substring start end))))

(global-set-key (kbd "C-c w b") 'ix-io-paste-buffer)
(global-set-key (kbd "C-c w r") 'ix-io-paste-region)

;;;; term/ansi-term
;; show URLs
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local kill-read-only-ok t)
            (goto-address-mode +1)))

;; please let me cut and paste, and other normal things
(add-hook 'term-load-hook
          (lambda ()
            (define-key term-raw-map (kbd "M-:") 'eval-expression)
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "C-k")
              (lambda ()
                (interactive)
                (term-send-raw-string "\C-k")
                (kill-line)))))

;; always kill-buffer after exit
(advice-add 'term-handle-exit :filter-return #'kill-buffer)

(global-set-key (kbd "C-c v")
                (lambda ()
                  (interactive)
                  (if (get-buffer "*ansi-term*")
                      (switch-to-buffer "*ansi-term*")
                    (ansi-term "/run/current-system/sw/bin/bash"))))

;;; Modes
;;;; common config for all prog-modes
(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode +1)
            (display-line-numbers-mode +1)
            (setq show-trailing-whitespace t)))

;;;; hl-nums, hl-bool, hl-todo
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[^a-zA-Z]\\(\\(\+\\|-\\|[0-9]+\.\\)?[0-9]+\\)[^a-zA-Z]"
                1 font-lock-constant-face)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[^a-zA-Z]\\(t\\|nil\\)[^a-zA-Z]"
                1 font-lock-constant-face)))))

(defface highlight-todo-face
  '((t :inherit font-lock-warning-face
       :weight bold
       :slant italic))
  "Basic face for highlighting TODO &c.")

(defvar highlight-todo-face 'highlight-todo-face)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|IMPORTANT\\):"
                1 highlight-todo-face t)))))

;;;; C
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 8)
            (setq c-default-style "linux")
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

;;;; lisp
(setq inferior-lisp-program
      "sbcl --no-userinit --eval \"(require 'sb-aclrepl)\"")
(global-set-key (kbd "C-c l") 'run-lisp)

;;;; scheme
(setq scheme-program-name "csi")
(global-set-key (kbd "C-c s") 'run-scheme)

;; in eshell comments aren't highlighted. In run-scheme CHICKEN Scheme's
;; prompt gets incorrectly highlighted as a comment *after* the face to
;; highlight it as a prompt is set. This ensures run-scheme's prompt
;; uses `comint-highlight-prompt'.
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (setq-local font-lock-comment-face nil)))

;;; Functions
;;;; BMI
;; Function to calculate body mass index (BMI). For problems/flaws, see:
;; https://en.wikipedia.org/wiki/Body_mass_index
(defun bmi (weight height)
  "Return BMI for WEIGHT at HEIGHT."
  (let* ((height (if (< height 3)
                     height
                   (/ height 100.0))))
    (string-to-number (format "%.2f" (/ weight (* height height))))))

;;;; completion-in-region-function
(defun completing-read-completion--in-region (start end coll &optional pred)
  "Use 'completing-read' to select a completion in region.

START and END are the position in the buffer of the string to be
completed.  COLL is the collection of possible completions, and
PRED limits the possible completions to a subset of COLL.

The following are tried in order:

 - in a minibuffer, run the 'completion--in-region' function

 - if there is only one possible completion, insert it

 - run 'completing-read' with a list of possible completions, and
   insert that."
  (if (minibufferp)
      (completion--in-region start end coll pred)
    (let* ((init (buffer-substring-no-properties start end))
           (comp (all-completions init coll pred))
           (meta (completion-metadata-get
                  (completion-metadata init coll pred) 'category)))
      (pcase (safe-length comp)
        (`0)
        (`1 (let ((minibuffer-completion-table coll)
                  (minibuffer-completion-predicate pred))
              (completion--in-region-1 start end)))
        ( _ (let ((sel
                   (if (eq meta 'file)
                       (replace-regexp-in-string
                        "^\\\\~/" "~/"
                        (replace-regexp-in-string
                         "\\\\ $" " "
                         (shell-quote-argument
                          (read-file-name "Completions: "
                                          (file-name-directory init) init nil
                                          (file-name-nondirectory init) pred))))
                     (completing-read "Completions: " coll pred nil init))))
              (when sel
                (completion--replace start end sel))))))))

;;; end initialisation
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))

(provide 'init)
;;; init.el ends here
