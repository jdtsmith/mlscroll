;;; mlscroll.el --- A scroll bar for the modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/mlscroll
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.3
;; Keywords: convenience
;; Prefix: mlscroll
;; Separator: -

;; MLScroll is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; MLScroll is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; MLScroll provides a small and lean graphical text based scrollbar
;; for the mode line.  To use, simply load the package and add:
;;
;;    (mlscroll-mode 1)
;;
;; to your configuration.  To toggle at any time:
;;
;;    M-x mlscroll-mode.

(require 'seq)
(require 'cl-lib)

;;; Code:
(defgroup mlscroll nil
  "Mode-line Scrolling."
  :prefix "mlscroll"
  :group 'mode-line
  :tag "Mode-line scroll")

(defcustom mlscroll-alter-percent-position t
  "Whether to remove or replace the mode line percentage position.
Removes if t, replaces if set to the symbol 'replace (but only
if `mlscroll-right-align' is nil).  Only effective if
`mode-line-percent-position' is at the beginning of
`mode-line-position'."
  :group 'mlscroll
  :type '(choice
	  (const :tag "do nothing" nil)
	  (const :tag "remove" t)
	  (const :tag "replace" replace)))

(defcustom mlscroll-right-align t
  "Whether to right-align the scrollbar within the mode line.
If set to nil, you must arrange to include
'(eval: (mlscroll-mode-line)) somewhere in `mode-line-format'."
  :group 'mlscroll
  :type 'boolean)

(defcustom mlscroll-in-color (face-attribute 'region :background nil t)
  "Background color for range inside of current window bounds."
  :group 'mlscroll
  :type 'color)

(defcustom mlscroll-out-color (face-attribute 'default :background)
  "Background color for range outside of current window bounds."
  :group 'mlscroll
  :type 'color)

(defcustom mlscroll-width-chars 12
  "Width of the mode line scroll indicator in characters.
Default is 12 characters wide."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-minimum-current-width 2
  "Minimum pixel width of the current window region (central) bar.
In the terminal, where a 'pixel' is one character, defaults to 1."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-border nil
  "Border in pixels around the scrollbar.
Drawn in the mode line's background color.  Defaults to 1/4 of the
default font's character height."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-shortfun-min-width nil
  "If non-nil, truncate `which-function' to a minimum of this width in chars.
If Which-Function mode is enabled, setting this option will
truncate the current function name from the right, down to the
specified width.  This allows the scroll bar to appear fully on
the mode line in more situations."
  :group 'mlscroll
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "Minimum Width")))

;(defvar-local mlscroll-cache-stats [0 0 0])
(defvar-local mlscroll-linenum-cache '((0 0 0) 0 0 0)
  "A per-buffer cache for line number lookup.
Format:
 ( (buf-tick point-min point-max)
   last-start-pos line-start line-max )")

(defun mlscroll-line-numbers (&optional win)
  "Calculate and return line numbers.
Returns a list of 3 line numbers at: window start, window end,
and `point-max'.  Uses caching for speed.  If WIN is passed, use the
window limits and `point-max' of the buffer in that window."
  (with-current-buffer (window-buffer win)
    (let* ((mod (car mlscroll-linenum-cache))
	   (last-bt (car mod)) (last-pmn (nth 1 mod)) (last-pmx (nth 2 mod))
	   (cache (cdr mlscroll-linenum-cache))
	   (old-start (car cache))
	   (old-line-start (nth 1 cache))
	   (old-line-max (nth 2 cache))
	   (wstart (window-start win)) (wend (window-end win))
	   (pmn (point-min)) (pmx (point-max))
	   (buf-tick (buffer-modified-tick))
	   (line-number-display-limit-width 2000000)
	   lstart lend lmax)
      (if (eq wend nil) (setq wend (window-end win t)))
      (if (and (= buf-tick last-bt) (= pmn last-pmn) (= pmx last-pmx))
	  (setq lstart (cond ((= wstart old-start) old-line-start)
			     ((< (abs (- wstart old-start)) (- wstart pmn -1000))
			      (funcall (if (> wstart old-start) #'+ #'-)
				       old-line-start
				       (count-lines wstart old-start)))
			     (t (line-number-at-pos wstart)))
		lend (+ lstart (count-lines wstart wend))
		lmax old-line-max)
	(setq mod (list buf-tick pmn pmx)
	      lstart (line-number-at-pos wstart)
	      lend (+ lstart (count-lines wstart wend))
	      lmax (+ lend   (count-lines wend pmx))))
      (setq mlscroll-linenum-cache (list mod wstart lstart lmax))
      (list lstart lend lmax))))

(defun mlscroll-scroll-to (x &optional idx win)
  "Scroll to the position identified by position X and component IDX.
IDX, if set, is an integer -- 0, 1, or 2 -- identifying which
component of the scrollbar we are in (0=left, 1=current,
2=right).  If IDX is non-nil, X is a pixel position within that
component, starting from 0 at left, _including_ border (for idx=1
only).  If IDX is nil, X can be either a symbol or an integer.
If it's the symbol 'up or 'down, the window is scrolled up or
down by half its height.  Otherwise, X is interpreted as a pixel
position within the entire scrollbar (_not_ including borders).
If WIN is set, it is a window whose buffer should be scrolled.
Returns the absolute x position within the full bar (with border
width removed)."
  (pcase-let* ((`(,left ,cur ,right ,start ,end ,last)
		(mlscroll--part-widths win))
	       (border (caddr (terminal-parameter nil 'mlscroll-size)))
	       (barwidth (+ left cur right))
	       (xpos (cond ((symbolp x) ; scroll wheel
			    (+ left (if (eq x 'down)
					(- (/ (float cur) 2))
				      (/ (* (float cur) 3) 2))))
			   ((null idx) x)
			   ((= idx 0) (- x border))
			   ((= idx 1) (+ x left))
			   ((= idx 2) (+ x left cur))
			   (t x)))
	       (frac (max 0 (min 1 (/ (float xpos) barwidth))))
	       (targ (- (1+ (round (* frac last)))
			(/ (- end start -1) 2))))
    (when (/= targ start)
      (set-window-start
       win
       (with-current-buffer (window-buffer win)
	 (save-excursion
	   (goto-char (cadr mlscroll-linenum-cache))
	   (forward-line (- targ start))
	   (point)))))
    xpos))

(defun mlscroll-wheel (event)
  "Handle mouse scroll events on the scroll bar.
EVENT is the mouse scroll event."
  (interactive "e")
  (let ((type (event-basic-type event))
	(win (posn-window (event-start event))))
    (mlscroll-scroll-to
     (if (eq type mouse-wheel-up-event) 'up 'down)
     nil win)))

(defun mlscroll-find-index (posn-string)
  "Find the 0-based index of the POSN-STRING position within the scroll parts."
  (let ((string (car posn-string))
	(pos (cdr posn-string)))
    (if (and (/= pos 0)
	     (get-text-property (1- pos) 'mlscroll string))
	(- pos (previous-single-property-change pos 'mlscroll string 0))
      0)))

(defun mlscroll-mouse (start-event)
  "Handle click and drag mouse events on the mode line scroll bar.
START-EVENT is the automatically passed mouse event."
  (interactive "e")
  (let* ((start-posn (event-start start-event))
	 (start-win (posn-window start-posn))
	 (pstring (posn-string start-posn))
	 (lcr (mlscroll-find-index pstring))
	 (x (car (posn-object-x-y start-posn))) ;; Doesn't function for xterm-mouse-mode
	 (xstart-abs (car (posn-x-y start-posn)))
	 (xstart (mlscroll-scroll-to x lcr start-win))
	 event end xnew)
    ;; (message "GOT[%d]: %S" lcr
    ;; 	     (pcase-let ((`(,sx . ,sy) (posn-x-y start-posn))
    ;; 			 (`(,ex . ,ey) (posn-x-y (posn-at-point (posn-point start-posn)))))
    ;; 	       (list (posn-area start-posn) (posn-x-y start-posn) (posn-point start-posn)
    ;; 		     sx sy ex ey ;; (- ex sx)
    ;; 		     ;; (- ey sy)
    ;; 		     )))
    (unless (terminal-parameter nil 'xterm-mouse-mode)
      (pcase-let ((`(,_ ,scroll-width ,border)
		   (terminal-parameter nil 'mlscroll-size))
		  (mouse-fine-grained-tracking t))
	(track-mouse
	  (setq track-mouse 'dragging)
	  (while (and (setq event (read-event))
		      (mouse-movement-p event))
	    (setq end (event-end event)
		  xnew (+ xstart (- (car (posn-x-y end)) xstart-abs)))
	    (when (and
		   (eq (posn-area end) 'mode-line)
		   (>= xnew 0)
		   (<= xnew (- scroll-width border)))
	      (mlscroll-scroll-to xnew nil start-win))))))))

(defun mlscroll--part-widths (&optional win)
  "Pixel widths of the bars (not including border).
Also returns line numbers at window start & end and (point-max).
If optional argument WIN is passed, it should be a window in
which to evaluate the line positions."
  (let* ((lines (mlscroll-line-numbers win))
	 (start (car lines))
	 (end (nth 1 lines))
	 (last (nth 2 lines))
	 (sizes (terminal-parameter nil 'mlscroll-size))
	 (scroll-width (cadr sizes))
	 (border (caddr sizes))
	 (w (- scroll-width (* 2 border)))
	 (cur (max (if (display-graphic-p) mlscroll-minimum-current-width 1)
		   (round
		    (* w (/ (float (- end start -1))
			    last)))))
	 (left (if (and (= start 1) (= last end)) 0
		 (max 0 (round (* (- w cur)
				  (/ (- start 1.0)
				     (+ (- start 1) (- last end))))))))
	 (right (max 0 (- w cur left))))
    (list left cur right start end last)))

(defvar mlscroll-flank-face-properties nil)
(defvar mlscroll-cur-face-properties nil)
(defconst mlscroll-extra-properties
  `(local-map
    (keymap (mode-line keymap
		       (down-mouse-1 . mlscroll-mouse)
		       (wheel-left . ignore)
		       (wheel-right . ignore)
		       (,mouse-wheel-up-event . mlscroll-wheel)
		       (,mouse-wheel-down-event . mlscroll-wheel)))
    help-echo "mouse-1: scroll buffer"
    mlscroll t))

(defvar mlscroll-shortfun-mlparts nil
  "Separate parts of the mode line for use when function shortening is enabled.")
(defvar mlscroll-shortfun-remain nil)
(defun mlscroll-shortfun-modeline ()
  "Mode line replacement for shortening which-func."
  (let* ((first (format-mode-line (car mlscroll-shortfun-mlparts)))
	 (cur-length (string-width first))
	 (char-width (car (terminal-parameter nil 'mlscroll-size)))
	 (ww (window-width nil t))
	 (remain
	  (max mlscroll-shortfun-min-width
	       (- (/ ww char-width)
		  cur-length
		  mlscroll-width-chars 3)))) ; 2 = [, ] + 1 for padding
    `(,first
      (:eval (let ((mlscroll-shortfun-remain ,remain)) ; let bind with only the
	       (format-mode-line mode-line-misc-info))); symbol doesn't work
      ,@(cdadr mlscroll-shortfun-mlparts)))) ; all the rest

(defvar mlscroll-shortfun-saved nil)
(defvar which-func-current)
(defvar which-func-format)
(defun mlscroll-shortfun-unsetup ()
  "Reverse the setup."
  (when mlscroll-shortfun-saved
    (setq-default mode-line-format (car mlscroll-shortfun-saved))
    (setq which-func-format (cdr mlscroll-shortfun-saved))))

(defun mlscroll-shortfun-setup ()
  "Setup MLScroll."
  (if mlscroll-shortfun-min-width
      (let ((mlmi-pos (seq-position mode-line-format 'mode-line-misc-info)))
	(if (not mlmi-pos)
	    (error "mode-line-misc-info not in mode-line-format -- cannot use mlscroll-shortfun-min-width")
	  (setq mlscroll-shortfun-saved (cons mode-line-format
					      which-func-format)
		mlscroll-shortfun-mlparts (seq-partition mode-line-format
							 mlmi-pos))
	  (setq-default mode-line-format '(:eval (mlscroll-shortfun-modeline)))
	  (cl-nsubst "" " " mode-line-misc-info :test #'equal)
	  (cl-nsubst `(:eval
		       (let ((wfc ,(cadr which-func-current))) ; sans :eval
			 (if (and wfc mlscroll-shortfun-remain
				  (> (length wfc) mlscroll-shortfun-remain))
			     (concat "…"
				     (substring wfc (- 1 mlscroll-shortfun-remain)))
			   wfc)))
		     'which-func-current which-func-format)))))

(defun mlscroll-mode-line ()
  "Generate text for mode line scrollbar.
Intended to be set in an :eval in the mode line, e.g. (as is done
by default if `mlscroll-right-align' is non-nil), in
`mode-line-end-spaces'."
  (pcase-let* ((`(,left ,cur ,right) (mlscroll--part-widths))
	       (`(,_ ,scroll-width ,scroll-border) (terminal-parameter nil 'mlscroll-size))
	       (bar (concat
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display
				 `(space :width (,(+ left scroll-border))))
		     (propertize " " 'face mlscroll-cur-face-properties
				 'display
				 `(space :width (,cur)))
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display
				 `(space :width (,(+ right scroll-border)))))))
    (add-text-properties 0 (length bar) mlscroll-extra-properties bar)
    (if mlscroll-right-align
	(list
	 (propertize " " 'display ; spacer -- align right
		     `(space :align-to (- (+ right right-margin)
					  (,(- scroll-width scroll-border)))))
	  bar)
      bar)))

(defvar mlscroll-saved [nil nil]
  "Saved parts of mode line.")

(defun mlscroll--update-size (&optional frame)
  "Update terminal parameter for terminal of FRAME with scrollbar size info.
Defaults to the current frame.  A list with 3 sizes is saved:
  (font-width scrollbar-width and scrollbar-border)"
  (let ((fw (or (and (display-graphic-p frame) (display-multi-font-p frame)
		 (let ((mlw (aref (font-info (face-font 'mode-line)) 11)))
		   (if (> mlw 1) mlw))) ; sometimes mode-line font fails
		(with-selected-window (frame-first-window frame)
		  (default-font-width)))))
    (set-terminal-parameter frame 'mlscroll-size
			    (list fw (* fw mlscroll-width-chars)
				  (if (display-graphic-p frame) mlscroll-border 0)))))

;;;###autoload
(define-minor-mode mlscroll-mode
  "Minor mode for displaying an interactive scrollbar in the mode line."
  :global t
  (if mlscroll-mode
      (let ((mode-line-has-box
	     (or (and (face-attribute 'mode-line :box)
		      (not (eq 'unspecified
			       (face-attribute 'mode-line :box))))
		 (and (face-attribute 'mode-line-inactive :box)
		      (not (eq 'unspecified
			       (face-attribute 'mode-line-inactive :box)))))))
	(add-hook 'after-make-frame-functions #'mlscroll--update-size)
	(unless (or mlscroll-border mode-line-has-box)
	  (setq mlscroll-border (floor (/ (float (default-font-height)) 4))))
	(when (and mlscroll-border (> mlscroll-border 0) mode-line-has-box)
	  (message "MLScroll border is incompatible with mode-line :box, disabling")
	  (setq mlscroll-border 0))
	(unless mlscroll-border (setq mlscroll-border 0))
	(mlscroll--update-size)
	(if (and mlscroll-border (> mlscroll-border 0))
	    (setq mlscroll-flank-face-properties        ; For box to enclose all 3 segments
		  `(:foreground ,mlscroll-out-color     ; (no internal borders) , they must
		    :box (:line-width ,mlscroll-border) ; have the same :foreground
		    :inverse-video t)			; (after inversion)
		  mlscroll-cur-face-properties
		  `(:foreground ,mlscroll-in-color
		    :box (:line-width ,mlscroll-border)
		    :inverse-video t))
	  (setq mlscroll-flank-face-properties
		`(:background ,mlscroll-out-color)
		mlscroll-cur-face-properties
		`(:background ,mlscroll-in-color)))

	(when mlscroll-right-align
	  (when (eq mlscroll-alter-percent-position 'replace)
	    (message "MLScroll: cannot both right-align and replace percent position, disabling replace")
	    (setq mlscroll-alter-percent-position t)) ; remove only
	  (setf (aref mlscroll-saved 1) mode-line-end-spaces
		mode-line-end-spaces '(:eval (mlscroll-mode-line))))

	(if (and mlscroll-alter-percent-position
		 (eq (cadar mode-line-position) 'mode-line-percent-position))
            (setf (aref mlscroll-saved 0) (car mode-line-position)
		  mode-line-position
		  (if (eq mlscroll-alter-percent-position 'replace) ; put MLScroll there!
		      (cons '(:eval (mlscroll-mode-line)) (cdr mode-line-position))
		    (cdr mode-line-position))))
	(if mlscroll-shortfun-min-width (mlscroll-shortfun-setup)))
    ;; Disabling
    (remove-hook 'after-make-frame-functions #'mlscroll--update-size)
    (mlscroll-shortfun-unsetup)
    (if (aref mlscroll-saved 1)
	(setq mode-line-end-spaces (aref mlscroll-saved 1)))
    (if (aref mlscroll-saved 0)
	(if (eq mlscroll-alter-percent-position 'replace)
	    (setcar mode-line-position (aref mlscroll-saved 0))
	  (setq mode-line-position (cons (aref mlscroll-saved 0)
					 mode-line-position))))
    (setq mlscroll-saved [nil nil])))

(provide 'mlscroll)
;;; mlscroll.el ends here
