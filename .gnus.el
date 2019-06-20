;; email
(setq gnus-select-method '(nnimap "email"
                                  (nnimap-address "mail.gandi.net")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl))
      ;; use smtp to send email
      send-mail-function 'smtpmail-send-it
      ;; smtp
      smtpmail-smtp-server "mail.gandi.net"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      ;; make subbed groups visible
      gnus-permanently-visible-groups "INBOX\\|Sent\\|archive\\|cyber"
      gnus-use-cache t
      ;; copy sent emails to Sent
      gnus-message-archive-group "nnimap+email:Sent"
      gnus-gcc-mark-as-read t
      message-directory "~/.emacs.d/mail/"
      nnfolder-directory "~/.emacs.d/mail/archive"
      mm-text-html-renderer 'gnus-w3m)

;; news
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))

(global-display-line-numbers-mode -1)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; check for news, notify
(gnus-demon-add-handler 'gnus-demon-scan-mail 5 t)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
