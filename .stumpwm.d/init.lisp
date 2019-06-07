;; -*- lisp -*-

;; general stuffs
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "feh --bg-fill '/home/cjb/pictures/desktop.jpg'")
(set-prefix-key (kbd "C-z"))
(setf *window-border-style* :none)
(setf *normal-border-width* 0)
(set-font "-*-terminus-medium-r-normal--16-*")

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
  "turn down the volume with amixer, then display volume"
  (run-shell-command "amixer set Master unmute; amixer set Master 2%-")
  (run-shell-command "amixer get Master | grep 'Mono: Pl' | cut -d' ' -f6" t))

(defcommand volume-up () ()
  "turn up the volume with amixer, then display volume"
  (run-shell-command "amixer set Master unmute; amixer set Master 2%+")
  (run-shell-command "amixer get Master | grep 'Mono: Pl' | cut -d' ' -f6" t))

(defcommand toggle-mute () ()
  "toggle mute with amixer, then display state"
  (run-shell-command "amixer set Master toggle")
  (run-shell-command "amixer get Master | grep 'Mono: Pl' | cut -d' ' -f8" t))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "toggle-mute")

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
