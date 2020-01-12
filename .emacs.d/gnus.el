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
      gnus-asynchronous t
      gnus-use-cache 'passive
      ;; copy sent emails to Sent
      gnus-message-archive-group "nnimap+email:Sent"
      gnus-gcc-mark-as-read t
      message-directory "~/.emacs.d/mail/"
      nnfolder-directory "~/.emacs.d/mail/archive"
      mm-text-html-renderer 'gnus-w3m)

(global-display-line-numbers-mode -1)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

(require 'notifications)

;; TODO: make this only notify for *new* emails, and not all unread emails
(defun my/gnus-notify ()
  "send a new emails notification"
  ;; YUK.
  (setq my/email-count (gnus-group-unread "INBOX"))
  (notifications-notify
   :title "Gnus"
   :body (format "You have %s new %s!" my/email-count (if (= my/email-count 1)
                                                          "email"
                                                        "emails"))
   :app-icon nil))

(add-hook 'gnus-after-getting-new-news-hook 'my/gnus-notify t)

;; setup this demon *after* gnus has loaded, otherwise it does not work
(with-eval-after-load "gnus"
  (setq gnus-demon-timestep 1)
  (gnus-demon-add-handler 'gnus-demon-scan-news 60 t))
