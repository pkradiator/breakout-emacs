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

(defcustom breakout-bat-width 5
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

(defcustom breakout-timer-delay 0.05
  "Time to wait between every cycle."
  :type 'number)

(defcustom breakout-sidebar-width 15
  "Width of the sidebar for score and lives."
  :type '(integer))

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

(defvar breakout-brick-red-options
  '(((glyph colorize) (t ?\#))
    ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
    (((glyph color-x) [1 0 0]) (color-tty "red"))))

(defvar breakout-brick-orange-options
  '(((glyph colorize) (t ?\#))
    ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
    (((glyph color-x) [1 0.5 0]) (color-tty "orange"))))

(defvar breakout-brick-yellow-options
  '(((glyph colorize) (t ?\#))
    ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
    (((glyph color-x) [1 1 0]) (color-tty "yellow"))))

(defvar breakout-brick-green-options
  '(((glyph colorize) (t ?\#))
    ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
    (((glyph color-x) [0 1 0]) (color-tty "green"))))

(defvar breakout-brick-blue-options
  '(((glyph colorize) (t ?\#))
    ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
    (((glyph color-x) [0 0 1]) (color-tty "blue"))))

(defconst breakout-blank  0)
(defconst breakout-bat	  1)
(defconst breakout-ball	  2)
(defconst breakout-border 3)
(defconst breakout-brick-red    4)
(defconst breakout-brick-orange 5)
(defconst breakout-brick-yellow 6)
(defconst breakout-brick-green  7)
(defconst breakout-brick-blue   8)

(defvar breakout-bricks-count 0
  "Number of bricks remaining.")


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
(defvar breakout-lives nil)

;;; Initialize maps

(defvar-keymap breakout-mode-map
  :doc "Modemap for breakout-mode."
  :name 'breakout-mode-map
  "<left>"       #'breakout-move-left
  "<right>"      #'breakout-move-right
  breakout-left-key  #'breakout-move-left
  breakout-right-key #'breakout-move-right
  breakout-quit-key  #'breakout-quit
  breakout-pause-key #'breakout-pause
  "n"            #'breakout)

(defvar-keymap breakout-null-map
  :doc "Null map for breakout-mode."
  :name 'breakout-null-map)


;;; Fun stuff -- The code

(define-derived-mode breakout-mode special-mode "Breakout"
  "Major mode for playing Breakout."
  (setq-local gamegrid-use-glyphs t)
  (setq-local gamegrid-use-color t))

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
                  ((= c breakout-brick-red)
                   breakout-brick-red-options)
                  ((= c breakout-brick-orange)
                   breakout-brick-orange-options)
                  ((= c breakout-brick-yellow)
                   breakout-brick-yellow-options)
                  ((= c breakout-brick-green)
                   breakout-brick-green-options)
                  ((= c breakout-brick-blue)
                   breakout-brick-blue-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun breakout-draw-sidebar ()
  "Draw score and lives in the sidebar."
  (let ((x-pos (+ breakout-width 2))
        (score-str (format "Score: %d" breakout-score))
        (lives-str (format "Lives: %d" breakout-lives)))
    (dotimes (i (length score-str))
      (gamegrid-set-cell (+ x-pos i) 2 (aref score-str i)))
    (dotimes (i (length lives-str))
      (gamegrid-set-cell (+ x-pos i) 4 (aref lives-str i)))))

(defun breakout-init-buffer ()
  "Initialize breakout buffer and draw stuff thanks to gamegrid library."
  (get-buffer-create breakout-buffer-name)
  (switch-to-buffer breakout-buffer-name)
  (breakout-mode)

  (setq gamegrid-use-glyphs t)
  (setq gamegrid-use-color t)
  (gamegrid-init (breakout-display-options))

  (gamegrid-init-buffer (+ breakout-width breakout-sidebar-width)
			breakout-height
			?\s)

  (let ((buffer-read-only nil))
    (dotimes (y breakout-height)
      (dotimes (x (+ breakout-width breakout-sidebar-width))
        (gamegrid-set-cell x y breakout-blank)))

    ;; Top border (playfield only)
    (dotimes (x breakout-width)
      (gamegrid-set-cell x 0 breakout-border))
    ;; Side borders (playfield)
    (dotimes (y breakout-height)
      (gamegrid-set-cell 0 y breakout-border)
      (gamegrid-set-cell (1- breakout-width) y breakout-border))

    ;; Separator for sidebar
    (dotimes (y breakout-height)
      (gamegrid-set-cell breakout-width y breakout-border))

    ;; Bricks
    (setq breakout-bricks-count 0)
    (cl-loop for (y . color) in `((3 . ,breakout-brick-red)
                                  (4 . ,breakout-brick-orange)
                                  (5 . ,breakout-brick-yellow)
                                  (6 . ,breakout-brick-green)
                                  (7 . ,breakout-brick-blue))
             do (cl-loop for x from 2 to (- breakout-width 3)
                         do (progn
                              (gamegrid-set-cell x y color)
                              (cl-incf breakout-bricks-count)))))

  (breakout-draw-sidebar)
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
  (gamegrid-start-timer breakout-timer-delay 'breakout-update-game))

(defun breakout-update-game (breakout-buffer)
  "\"Main\" function for breakout.
It is called every breakout-timer-delay seconds and
updates ball and bats positions.  It is responsible of collision
detection and checks if the player scores."
  (if (not (eq (current-buffer) breakout-buffer))
      (breakout-pause)
    (let* ((old-x breakout-x)
	   (old-y breakout-y))

      (gamegrid-set-cell old-x old-y breakout-blank)

      ;; 1. Horizontal wall collision
      (when (or (<= (+ old-x breakout-xx) 0)
                (>= (+ old-x breakout-xx) (1- breakout-width)))
        (setq breakout-xx (- breakout-xx)))

      ;; 2. Vertical wall collision (top)
      (when (<= (+ old-y breakout-yy) 0)
        (setq breakout-yy (- breakout-yy)))

      ;; Recalculate intended position after wall bounces
      (let* ((nx (+ old-x breakout-xx))
             (ny (+ old-y breakout-yy))
             (cell (gamegrid-get-cell nx ny)))

        ;; 3. Collision with bricks
        (when (and (>= cell breakout-brick-red)
                   (<= cell breakout-brick-blue))
          (gamegrid-set-cell nx ny breakout-blank)
          (setq breakout-yy (- breakout-yy))
          (setq breakout-score (+ breakout-score 10))
          (setq breakout-bricks-count (1- breakout-bricks-count))
          (breakout-draw-sidebar)
          (when (= breakout-bricks-count 0)
            (gamegrid-kill-timer)
            (message "You Win! Final Score: %d. Press 'n' for new game." breakout-score))
          ;; Update ny for subsequent checks
          (setq ny (+ old-y breakout-yy)))

        ;; 4. Collision with bat
        (when (and (= ny (- breakout-height 3))
                   (>= nx breakout-bat-pos)
                   (< nx (+ breakout-bat-pos breakout-bat-width)))
          (setq breakout-yy (- breakout-yy))
          (let ((bat-mid (+ breakout-bat-pos (/ breakout-bat-width 2))))
            (cond
             ((> nx bat-mid) (setq breakout-xx 1))
             ((< nx bat-mid) (setq breakout-xx -1))
             (t (setq breakout-xx 0))))
          ;; Update positions after bat bounce
          (setq nx (+ old-x breakout-xx)
                ny (+ old-y breakout-yy)))

        ;; 5. Final movement / Loss condition
        (if (>= ny (1- breakout-height))
            (progn
              (setq breakout-lives (1- breakout-lives))
              (breakout-draw-sidebar)
              (if (<= breakout-lives 0)
                  (progn
                    (gamegrid-kill-timer)
                    (message "Game Over! Final Score: %d. Press 'n' for new game." breakout-score))
                (progn
                  (message "Life lost! Lives left: %d." breakout-lives)
                  (breakout-reset-ball))))
          (progn
            (setq breakout-x nx
                  breakout-y ny)
            (gamegrid-set-cell breakout-x breakout-y breakout-ball)))))))

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


(defun breakout-reset-ball ()
  "Reset ball and bat to initial positions."
  (let ((buffer-read-only nil))
    ;; Clear old ball
    (gamegrid-set-cell breakout-x breakout-y breakout-blank)
    ;; Clear old bat
    (cl-loop for x from breakout-bat-pos
             to (1- (+ breakout-bat-pos breakout-bat-width))
             do (gamegrid-set-cell x (- breakout-height 3) breakout-blank))

    ;; Reset positions
    (setq breakout-bat-pos (1+ (/ (- breakout-width breakout-bat-width) 2)))
    (setq breakout-xx 0)
    (setq breakout-yy 1)
    (setq breakout-x (/ breakout-width 2))
    (setq breakout-y (/ breakout-height 2))

    ;; Draw new ball and bat
    (gamegrid-set-cell breakout-x breakout-y breakout-ball)
    (cl-loop for x from breakout-bat-pos
             to (1- (+ breakout-bat-pos breakout-bat-width))
             do (gamegrid-set-cell x (- breakout-height 3) breakout-bat))))

(defun breakout ()
  "Play breakout and waste time.
This is an implementation of the classical game breakout.
Move left, right and try to brack all the bricks.

breakout-mode keybindings:\\<breakout-mode-map>

\\{breakout-mode-map}"
  (interactive)
  (setq breakout-score 0)
  (setq breakout-lives 3)
  (breakout-init))
  
(provide 'breakout)

;;; breakout.el ends here
