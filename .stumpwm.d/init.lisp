;; load quicklisp for cool kids stuff
(load "~/quicklisp/setup.lisp")

(set-prefix-key (kbd "C-z"))
(setf *startup-message* nil
      *window-border-style* :none
      *message-window-gravity* :bottom-right
      *input-window-gravity* :bottom-right)

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
;; uncomment this for first run, or if you install a new font
;;(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "Iosevka Term Slab"
                         :subfamily "Regular"
                         :size 11))
