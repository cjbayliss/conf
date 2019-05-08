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
      gnus-permanently-visible-groups ""
      gnus-use-cache t
      ;; copy sent emails to Sent
      gnus-message-archive-group "nnimap+email:Sent"
      gnus-gcc-mark-as-read t)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
