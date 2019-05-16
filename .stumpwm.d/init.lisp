;;; -*- mode: common lisp -*-
;; ^ that is an attempt to make github change its mind, i'm positive this is
;; *not* NewLisp. 😑

;; load quicklisp for cool kids stuff
(load "~/quicklisp/setup.lisp")

;; general stuffs
(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr")
(stumpwm:run-shell-command "feh --bg-fill '/home/cjb/pictures/v0h9ok63q1y21.jpg'")
(set-prefix-key (kbd "C-z"))
(setf *window-border-style* :none)
(setf *normal-border-width* 0)

;; start dmenu with C-z C-d
(define-key *root-map* (kbd "C-d") "exec dmenu_run -b -fn 'Iosevka Term Slab'")
;; start terminal with C-z c or C-z C-c
(define-key *root-map* (kbd "c") "exec sst")
(define-key *root-map* (kbd "C-c") "exec sst")
;; start firefox with C-z C-f
(define-key *root-map* (kbd "C-f") "exec firefox")

;; brightness control
(defcommand brightness-down () ()
  "turn down the brightness with brightnessctl"
  (run-shell-command "brightnessctl set 5%-"))

(defcommand brightness-up () ()
  "turn up the brightness with brightnessctl"
  (run-shell-command "brightnessctl set +5%"))

(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-down")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-up")

;; volume control
(defcommand volume-down () ()
  "turn down the volume with amixer"
  (run-shell-command "amixer set Master unmute; amixer set Master 2%-"))

(defcommand volume-up () ()
  "turn up the volume with amixer"
  (run-shell-command "amixer set Master unmute; amixer set Master 2%+"))

(defcommand toggle-mute () ()
  "toggle mute with amixer"
  (run-shell-command "amixer set Master toggle"))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "mute-toggle")

;; cmus control
(defcommand music-next () ()
  "next song"
  (run-shell-command "cmus-remote --next"))

(defcommand music-prev () ()
  "next song"
  (run-shell-command "cmus-remote --prev"))

(defcommand music-pause () ()
  "next song"
  (run-shell-command "cmus-remote --pause"))

(define-key *top-map* (kbd "XF86AudioNext") "music-next")
(define-key *top-map* (kbd "XF86AudioPrev") "music-prev")
(define-key *top-map* (kbd "XF86AudioPlay") "music-pause")

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
;; uncomment this for first run, or if you install a new font
;;(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "Iosevka Term Slab"
                         :subfamily "Regular"
                         :size 11))
