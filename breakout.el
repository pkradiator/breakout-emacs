;;; breakout.el --- classical implementation of breakout  -*- lexical-binding:t -*-

;; Author: Priyanshu Kalal <kalalpriyanshu7@gmail.com>
;; Keywords: games

;;; Commentary:

;; This is an implementation of the classical game breakout.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;;; Customization

(defgroup breakout nil
  "Emacs lisp implementation of the classical game breakout."
  :tag "Breakout")

(defcustom breakout-buffer-name "*Breakout*"
  "Name of the buffer used to play."
  :type '(string))

(defcustom breakout-width 40
  "Width of the playfield."
  :type '(integer))

(defcustom breakout-height (min 35 (- (frame-height) 6))
  "Height of the playfield."
  :type '(integer))

(defcustom breakout-bat-width 3
  "Width of the bats for breakout."
  :type '(integer))

(defcustom breakout-blank-color "black"
  "Color used for background."
  :type 'color)

(defcustom breakout-bat-color "yellow"
  "Color used for bats."
  :type 'color)

(defcustom breakout-ball-color "red"
  "Color used for the ball."
  :type 'color)

(defcustom breakout-border-color "white"
  "Color used for breakout borders."
  :type 'color)

(defcustom breakout-left-key "4"
  "Alternate key to press for bat to go left (primary one is [left])."
  :type '(restricted-sexp :match-alternatives (stringp vectorp)))

(defcustom breakout-right-key "6"
  "Alternate key to press for bat 1 to go right (primary one is [right])."
  :type '(restricted-sexp :match-alternatives (stringp vectorp)))

(defcustom breakout-quit-key "q"
  "Key to press to quit breakout."
  :type '(restricted-sexp :match-alternatives (stringp vectorp)))

(defcustom breakout-pause-key "p"
  "Key to press to pause breakout."
  :type '(restricted-sexp :match-alternatives (stringp vectorp)))

(defcustom breakout-resume-key "p"
  "Key to press to resume breakout."
  :type '(restricted-sexp :match-alternatives (stringp vectorp)))

(defcustom breakout-timer-delay 0.1
  "Time to wait between every cycle."
  :type 'number)

;;; This is black magic.  Define colors used

(defvar breakout-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty breakout-blank-color))))

(defvar breakout-bat-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [1 1 0])
     (color-tty breakout-bat-color))))

(defvar breakout-ball-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty breakout-ball-color))))

(defvar breakout-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty breakout-border-color))))

(defconst breakout-blank  0)
(defconst breakout-bat	  1)
(defconst breakout-ball	  2)
(defconst breakout-border 3)


;;; Determine initial positions for bats and ball

(defvar breakout-xx nil
  "Horizontal speed of the ball.")

(defvar breakout-yy nil
  "Vertical speed of the ball.")

(defvar breakout-x nil
  "Horizontal position of the ball.")

(defvar breakout-y nil
  "Vertical position of the ball.")

(defvar breakout-bat-pos nil
  "Horizontal position of bat.")

(defvar breakout-score nil)

;;; Initialize maps

(defvar-keymap breakout-mode-map
  :doc "Modemap for breakout-mode."
  :name 'breakout-mode-map
  "<left>"       #'breakout-move-left
  "<right>"      #'breakout-move-right
  breakout-left-key  #'breakout-move-left
  breakout-right-key #'breakout-move-right
  breakout-quit-key  #'breakout-quit
  breakout-pause-key #'breakout-pause)

(defvar-keymap pong-null-map
  :doc "Null map for pong-mode."
  :name 'pong-null-map)


;;; Fun stuff -- The code

(defun breakout-display-options ()
  "Computes display options (required by gamegrid for colors)."
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
            (cond ((= c breakout-blank)
                   breakout-blank-options)
                  ((= c breakout-bat)
                   breakout-bat-options)
                  ((= c breakout-ball)
                   breakout-ball-options)
                  ((= c breakout-border)
                   breakout-border-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun breakout-init-buffer ()
  "Initialize breakout buffer and draw stuff thanks to gamegrid library."
  (get-buffer-create breakout-buffer-name)
  (switch-to-buffer breakout-buffer-name)
  (use-local-map breakout-mode-map)

  (setq gamegrid-use-glyphs t)
  (setq gamegrid-use-color t)
  (gamegrid-init (breakout-display-options))

  (gamegrid-init-buffer breakout-width
			(+ 2 breakout-height)
			?\s)

  (let ((buffer-read-only nil))
    (dotimes (y breakout-height)
      (dotimes (x breakout-width)
        (gamegrid-set-cell x y breakout-border)))
    (cl-loop for y from 1 to (- breakout-height 2) do
             (cl-loop for x from 1 to (- breakout-width 2) do
                      (gamegrid-set-cell x y breakout-blank))))
  (cl-loop for x from breakout-bat-pos
	   to (1- (+ breakout-bat-pos breakout-bat-width))
	   do (gamegrid-set-cell x (- breakout-height 3) breakout-bat)))

(setq breakout-bat-pos (/ breakout-width 2))

(defun breakout-move-left ()
  "Move bat to left."
  (interactive nil breakout-mode)
  (if (> breakout-bat-pos 1)
      (and
       (gamegrid-set-cell (1- (+ breakout-bat-pos breakout-bat-width)) (- breakout-height 3)
			  breakout-blank)
       (setq breakout-bat-pos (1- breakout-bat-pos))
       (gamegrid-set-cell breakout-bat-pos (- breakout-height 3)
			  breakout-bat))))

(defun breakout-move-right ()
  "Move bat to right."
  (interactive nil breakout-mode)
  (if (< (+ breakout-bat-pos breakout-bat-width) (- breakout-width 1))
      (and
       (gamegrid-set-cell breakout-bat-pos (- breakout-height 3)
			  breakout-blank)
       (gamegrid-set-cell (+ breakout-bat-pos breakout-bat-width) (- breakout-height 3)
			  breakout-bat)
       (setq breakout-bat-pos (1+ breakout-bat-pos)))))

(defun breakout-init ()
  "Initialize a game."

  (define-key breakout-mode-map breakout-pause-key 'breakout-pause)

  (add-hook 'kill-buffer-hook 'breakout-quit nil t)

  ;; Initialization of some variables
  (setq breakout-bat-pos (1+ (/ (- breakout-width breakout-bat-width) 2)))
  (setq breakout-xx 0)
  (setq breakout-yy 1)
  (setq breakout-x (/ breakout-width 2))
  (setq breakout-y (/ breakout-height 2))

  (breakout-init-buffer)
  (gamegrid-kill-timer)
  (gamegrid-start-timer breakout-timer-delay 'breakout-update-game)
  (breakout-update-score))
(defun breakout-pause ()
  "Pause the game."
  (interactive nil breakout-mode)
  (gamegrid-kill-timer)
  ;; Oooohhh ugly.  I don't know why, gamegrid-kill-timer don't do the
  ;; jobs it is made for.  So I have to do it "by hand".  Anyway, next
  ;; line is harmless.
  (cancel-function-timers 'breakout-update-game)
  (define-key breakout-mode-map breakout-resume-key 'breakout-resume))

(defun breakout-resume ()
  "Resume a paused game."
  (interactive nil breakout-mode)
  (define-key breakout-mode-map breakout-pause-key 'breakout-pause)
  (gamegrid-start-timer breakout-timer-delay 'breakout-update-game))

(defun breakout-quit ()
  "Quit the game and kill the breakout buffer."
  (interactive nil breakout-mode)
  (gamegrid-kill-timer)
  ;; Be sure not to draw things in another buffer and wait for some
  ;; time.
  (run-with-timer breakout-timer-delay nil 'kill-buffer breakout-buffer-name))


(defun breakout ()
  "Play breakout and waste time.
This is an implementation of the classical game breakout.
Move left, right and try to brack all the bricks.

breakout-mode keybindings:\\<breakout-mode-map>

\\{breakout-mode-map}"
  (interactive)
  (setq breakout-score 0)
  (breakout-init))
  
(provide 'breakout)

;;; breakout.el ends here
