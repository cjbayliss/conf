(setq gc-cons-threshold most-positive-fixnum)

(defvar my/tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(org-babel-load-file (concat user-emacs-directory "config.org"))

(setq file-name-handler-alist my/tmp--file-name-handler-alist)
