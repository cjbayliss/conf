;; we use this function to return a list of lines within a file
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer (insert-file-contents filePath) (split-string (buffer-string) "\n" t)))

;; ERC config.
(let ((acc (read-lines "~/.my-erc-account")))
  (setq erc-nick (car acc))  
  (setq erc-password (nth 1 acc)))
(use-package erc
  :preface
  (require 'erc-goodies)
  (defun my/erc-buffer-to-bottom ()
    "Keep the ERC buffer pined to the bottom of the screen."
    (set (make-local-variable 'scroll-conservatively) 100))
  (defun my/erc-buffer-size ()
    "resizes the ERC buffer to match the screen size."
    (setq erc-fill-column (- (window-width) 2)))
  :hook ((window-configuration-change . my/erc-buffer-size)
         (erc-mode . my/erc-buffer-to-bottom))
  :config
  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 15
        erc-server "chat.au.freenode.net"
        erc-autojoin-channels-alist
        '(("freenode.net" "#xebian")("oftc.net" "#debian-devel"))
        erc-prompt (lambda () (concat "[" (buffer-name) "]"))))
  (erc-scrolltobottom-enable)
  (erc-notifications-mode)
  (erc-spelling-mode)
  (global-display-line-numbers-mode 0)
  (global-hl-line-mode 0)

(use-package erc-hl-nicks :after erc)

;; fix ERC prompt colours
(custom-set-faces '(erc-prompt-face ((t (:foreground "brightwhite" :background nil :weight bold)))))
