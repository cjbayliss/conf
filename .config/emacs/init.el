(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(defvar my/tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(org-babel-load-file (concat user-emacs-directory "config.org"))

(setq gc-cons-threshold 800000)
(setq gc-cons-percentage 0.1)

(setq file-name-handler-alist my/tmp--file-name-handler-alist)
