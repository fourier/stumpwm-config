;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "s-u"))
;;(run-shell-command "setxkbmap -layout dvorak")
;; set keyboard layout
(run-shell-command "setxkbmap -layout dvorak,ru -option \"grp:alt_shift_toggle,caps:ctrl_modifier\"")

;; Set the modeline
(toggle-mode-line (current-screen) (current-head))

;; Set "focus follow mouse" focus policy
(setf *mouse-focus-policy* :sloppy)

;; Suppress frame indicator
(setf *suppress-frame-indicator* t)

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))


(defcommand single-screen () (:rest)
  "Switch to single display setup"
  (run-shell-command "xrandr --output DP-0 --off"))

(defcommand dual-screen () (:rest)
  "Switch to dual display setup"
  (run-shell-command "xrandr --output LVDS-0 --left-of DP-0 --output DP-0 --auto"))


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

;; Configuration
(define-key *top-map* (stumpwm:kbd "s-F1") "single-screen")
(define-key *top-map* (stumpwm:kbd "s-F2") "dual-screen")
;; (define-key *top-map* (stumpwm:kbd "s-F3") "gselect 3")
;; (define-key *top-map* (stumpwm:kbd "s-F4") "gselect 4")
;; (define-key *top-map* (stumpwm:kbd "s-F5") "gselect 5")
;; (define-key *top-map* (stumpwm:kbd "s-F6") "gselect 6")
;; (define-key *top-map* (stumpwm:kbd "s-F7") "gselect 7")
;; (define-key *top-map* (stumpwm:kbd "s-F8") "gselect 8")
;; (define-key *top-map* (stumpwm:kbd "s-F9") "gselect 9")
;; (define-key *top-map* (stumpwm:kbd "s-F10") "gselect 10")

;; window management
(define-key *top-map* (stumpwm:kbd "s-b") "pull-from-windowlist")
(define-key *root-map* (kbd "b") "pull-from-windowlist")
(define-key *root-map* (kbd "s-b") "pull-from-windowlist")
(define-key *top-map* (kbd "s-S-Up") "move-window up")
(define-key *top-map* (kbd "s-S-Down")  "move-window down")
(define-key *top-map* (kbd "s-S-Left")  "move-window left")
(define-key *top-map* (kbd "s-S-Right") "move-window right")

;; some applications bindings
(define-key *top-map* (stumpwm:kbd "s-e") "emacs")
(define-key *top-map* (stumpwm:kbd "s-S-Return") "exec urxvt")
(define-key *top-map* (stumpwm:kbd "s-p") "exec dmenu_run")
(define-key *top-map* (stumpwm:kbd "s-S-p") "exec dmenu_run")

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
(set-font "-*-clean-*-*-*-*-16-*-*-*-*-*-*-*")
;; (set-font "xft:Monospace:medium:size=14")
;; Position output and input in the center of the screen
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

;;; Define window placement policy...

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
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))

(define-frame-preference "Emacs"
  (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  (0 t t :create "emacs-dump" :class "Emacs"))


;; create Slynk server
(ql:quickload :slynk)
(slynk:create-server :port 4006 :dont-close t)
(message "Slynk server on port 4006 created")
