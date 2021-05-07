;; Various functions and configurations for Gnus
;;
;; Written in 2019, 2020, 2021 by Christopher Bayliss <cjb@cjb.sh>
;;
;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to the
;; public domain worldwide. This software is distributed without any
;; warranty.
;;
;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software. If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;; email
(setq
 gnus-select-method '(nnimap "email"
                             (nnimap-address "mail.gandi.net")
                             (nnimap-server-port 993)
                             (nnimap-stream ssl))

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

 ;; use smtp to send email
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "mail.gandi.net"
 smtpmail-smtp-service 587

 ;; make subbed groups visible
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
 gnus-permanently-visible-groups "INBOX\\|Sent\\|archive\\|cyber"
 gnus-asynchronous t
 gnus-use-cache 'passive

 ;; copy sent emails to Sent
 gnus-message-archive-group "nnimap+email:Sent"
 message-directory (concat user-emacs-directory "mail")
 nnfolder-directory (concat user-emacs-directory "mail/archive")
 gnus-gcc-mark-as-read t)

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-after-getting-new-news-hook
          'display-time-event-handler)
(add-hook 'gnus-group-mode-hook 'display-time-event-handler)

;; setup this demon *after* gnus has loaded, otherwise it does not work
(with-eval-after-load "gnus"
  (setq gnus-demon-timestep 1)
  (gnus-demon-add-handler 'gnus-demon-scan-news 60 t))
