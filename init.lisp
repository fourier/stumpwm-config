;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; load modules
(load-module "battery-portable")
(load-module "amixer")

;; change the prefix key to something else
(set-prefix-key (kbd "s-t"))

;; Set the modeline and its format

(setf *screen-mode-line-format* "^BBat %B^b | ^B%d^b | ^B%n^b : %w ")
;; default: (setf *time-modeline-string* "%a %b %e %k:%M:%S")
(setf *time-modeline-string* "%b %e %k:%M")
;; default: (setf *window-format* "%m%n%s%50t")
(setf *window-format* "%m%n%s%50t")
;; update modeline every 2 seconds
(setf *mode-line-timeout* 2)
;; turn on modeline
(toggle-mode-line (current-screen) (current-head))

;; Set "focus follow mouse" focus policy
(setf *mouse-focus-policy* :sloppy)

;; Suppress frame indicator
(setf *suppress-frame-indicator* t)

;; hack to enable scroll in some programs
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")


;; set keyboard layout
(defun set-xkb () 
  ;; Run this command to restore capslock: (run-shell-command "setxkbmap -option")
  (run-shell-command "setxkbmap -layout dvorak,ru -option \"grp:alt_shift_toggle,caps:ctrl_modifier\""))


;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))


(defcommand single-screen () (:rest)
  "Switch to single display setup"
  (run-shell-command "xrandr --output DP-0 --off")
  (set-xkb)
  (message "Single screen"))

(defcommand dual-screen () (:rest)
  "Switch to dual display setup"
  (run-shell-command "xrandr --output LVDS-0 --left-of DP-0 --output DP-0 --auto")
  (set-xkb)
  (message "Dual screen"))

(defcommand urxvt () (:rest)
  "Switch to URxvt" 
  (run-or-raise "urxvt" '(:class "URxvt")))

(defcommand capture-screen() (:rest)
  "Screenshot"
  (run-shell-command "scrot $HOME/Pictures/Screenshots/screen_%Y-%m-%d-%H-%M-%S.png -d 1"))

(defcommand capture-window() (:rest)
  "Screenshot of the current window"
  (run-shell-command "scrot $HOME/Pictures/Screenshots/window_%Y-%m-%d-%H-%M-%S.png -d 1-u"))

;; to undefine keys use undefine-key 

;; navigation between windows, like tabs
(define-key *top-map* (stumpwm:kbd "s-Left") "prev")
(define-key *top-map* (stumpwm:kbd "s-Right") "next")
;; Navigation between groups
(define-key *top-map* (stumpwm:kbd "s-0") "gselect 0")
(define-key *top-map* (stumpwm:kbd "s-1") "gselect 1")
(define-key *top-map* (stumpwm:kbd "s-2") "gselect 2")
(define-key *top-map* (stumpwm:kbd "s-3") "gselect 3")
(define-key *top-map* (stumpwm:kbd "s-4") "gselect 4")
(define-key *top-map* (stumpwm:kbd "s-5") "gselect 5")
(define-key *top-map* (stumpwm:kbd "s-6") "gselect 6")
(define-key *top-map* (stumpwm:kbd "s-7") "gselect 7")
(define-key *top-map* (stumpwm:kbd "s-8") "gselect 8")
(define-key *top-map* (stumpwm:kbd "s-9") "gselect 9")
;; Move to group
(define-key *top-map* (stumpwm:kbd "s-)") "gmove 0")
(define-key *top-map* (stumpwm:kbd "s-!") "gmove 1")
(define-key *top-map* (stumpwm:kbd "s-@") "gmove 2")
(define-key *top-map* (stumpwm:kbd "s-#") "gmove 3")
(define-key *top-map* (stumpwm:kbd "s-$") "gmove 4")
(define-key *top-map* (stumpwm:kbd "s-%") "gmove 5")
(define-key *top-map* (stumpwm:kbd "s-^") "gmove 6")
(define-key *top-map* (stumpwm:kbd "s-&") "gmove 7")
(define-key *top-map* (stumpwm:kbd "s-*") "gmove 8")
(define-key *top-map* (stumpwm:kbd "s-(") "gmove 9")


;; Configuration
(define-key *top-map* (stumpwm:kbd "s-F1") "single-screen")
(define-key *top-map* (stumpwm:kbd "s-F2") "dual-screen")

;; window management
(define-key *top-map* (stumpwm:kbd "s-b") "pull-from-windowlist")
(define-key *root-map* (kbd "b") "pull-from-windowlist")
(define-key *root-map* (kbd "s-b") "pull-from-windowlist")
(define-key *top-map* (kbd "s-S-Up") "move-window up")
(define-key *top-map* (kbd "s-S-Down")  "move-window down")
(define-key *top-map* (kbd "s-S-Left")  "move-window left")
(define-key *top-map* (kbd "s-S-Right") "move-window right")

;; Audio controls (based on amixer module)
(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-master-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-master-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-master-toggle")

;; some applications bindings
(define-key *top-map* (kbd "s-e") "emacs")
(define-key *top-map* (kbd "s-S-Return") "exec urxvt")
(define-key *top-map* (kbd "s-p") "exec dmenu_run")
(define-key *top-map* (kbd "s-S-p") "exec dmenu_run")
(define-key *root-map* (kbd "quoteleft") "urxvt")
(define-key *root-map* (kbd "s-quoteleft") "urxvt")
(define-key *top-map* (kbd "SunPrint_Screen") "capture-screen")
(define-key *top-map* (kbd "M-SunPrint_Screen") "capture-window")

  
;; (define-key *top-map* (stumpwm:kbd "s-Left") "move-focus left")
;; (define-key *top-map* (stumpwm:kbd "s-j") "move-focus down")
;; (define-key *top-map* (stumpwm:kbd "s-k") "move-focus up")
;; (define-key *top-map* (stumpwm:kbd "s-l") "move-focus right")
;; (define-key *top-map* (stumpwm:kbd "s-H") "move-window left")
;; (define-key *top-map* (stumpwm:kbd "s-J") "move-window down")
;; (define-key *top-map* (stumpwm:kbd "s-K") "move-window up")
;; (define-key *top-map* (stumpwm:kbd "s-L") "move-window right")
;; (define-key *top-map* (stumpwm:kbd "s-n") "pull-hidden-next")
;; (define-key *top-map* (stumpwm:kbd "s-p") "pull-hidden-previous")

;; Read some doc
;;(define-key *root-map* (kbd "d") "exec gv")
;; Browse somewhere
;;(define-key *root-map* (kbd "b") "colon1 exec x-www-browser http://www.")
;; Ssh somewhere
;;(define-key *root-map* (kbd "C-s") "colon1 exec x-terminal-emulator -e ssh ")
;; Lock screen
;;(define-key *root-map* (kbd "C-l") "exec xlock")



;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

;;(make-web-jump "google" "x-www-browser http://www.google.fr/search?q=")
;;(make-web-jump "imdb" "x-www-browser http://www.imdb.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
;;(define-key *root-map* (kbd "M-s") "google")
;;(define-key *root-map* (kbd "i") "imdb")

;; Message window font
;;(set-font "-*-clean-*-*-*-*-16-*-*-*-*-*-*-*")
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-microsoft-cp1251")
;; (set-font "xft:Monospace:medium:size=14")
;; Position output and input in the center of the screen
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

;; remove window borders
(setf *window-border-style* :none)
;; This setting would cause urxvt to fill the frame.
;; see https://github.com/stumpwm/stumpwm/issues/843
(setf *ignore-wm-inc-hints* t)

;;; Define window placement policy

;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  ;;(0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;  (1 t nil :class "XTerm")
  (0 t t :class "URxvt")
  (0 t t :class "Far2l"))

(define-frame-preference "Emacs"
  (0 t t :class "Emacs"))

(define-frame-preference "Browser"
  (0 t t :role "browser"))

(define-frame-preference "Chat"
  (0 t t :class "discord"))

(define-frame-preference "Lisp"
  (0 t t :class "Lispworks"))

(define-frame-preference "Games"
  (0 t t :class "uo.bin")
  (0 t t :class "client.exe"))


;; Create groups
(add-group (current-screen) "Emacs")
(add-group (current-screen) "Browser")
(add-group (current-screen) "Chat")
(add-group (current-screen) "Lisp")
(add-group (current-screen) "Games")
(add-group (current-screen) "Misc 1")
(add-group (current-screen) "Misc 2")
(add-group (current-screen) "Misc 3")
(add-group (current-screen) "Misc 4")

;; create Slynk server
(ql:quickload "slynk")
(slynk:create-server :port 4006 :dont-close t)


;; set keyboard layout
(set-xkb)
