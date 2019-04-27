(require 'erc-goodies)

(defun my/return-lines (file)
  "return list of lines from file"
  (with-temp-buffer (insert-file-contents file)
                    (split-string (buffer-string) "\n" t)))

;; ERC config
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

;; fix ERC prompt colours
(custom-set-faces '(erc-prompt-face ((t (:foreground "brightwhite" :background nil :weight bold))))
                  '(erc-timestamp-face ((t (:foreground "brightwhite" :weight bold))))
                  '(erc-input-face ((t (:foreground "white"))))
                  '(erc-my-nick-face ((t (:foreground "brightred" :weight bold)))))
