;;; mlscroll.el --- A scroll bar for the modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/mlscroll
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.1.0
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

;;; Code:
(defgroup mlscroll nil
  "Mode-line Scrolling"
  :prefix "mlscroll"
  :group 'mode-line
  :tag "Mode-line scroll")

(defcustom mlscroll-disable-percent-position t
  "Whether to disable percentage position in mode line format.
Only works if `mode-line-percent-position' is at the beginning of
`mode-line-position'."
  :group 'mlscroll
  :type 'boolean)

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

(defvar mlscroll-width nil
  "Scroll width in pixels.
Derived from `mlscroll-width-chars'.")
(defvar mlscroll-mode-line-font-width nil
  "Character width in mode line.")

(defcustom mlscroll-minimum-current-width
  (if (> (default-font-width) 1) 2 1) ;terminal chars = 1 "pixel" wide
  "Minimum pixel width of the current window region (central) bar."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-border (floor (/ (float (default-font-height)) 4))
  "Border in pixels around the scrollbar.
Drawn in the mode line's background color.  Defaults to 1/4 of the
default font's character height."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-shortfun-min-width nil
  "If non-nil, the truncate which-function to a min of this width.
If which-function-mode is enabled, setting this option enables
truncating the current function name from the right, down to the
specified width.  This allows the scroll bar to appear fully on
the mode line in more situations."
  :group 'mlscroll
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "Minimum Width")))

;(defvar-local mlscroll-cache-stats [0 0 0])
(defvar-local mlscroll-linenum-cache '((0 0 0) 0 0 0)
  "A per-buffer cache for line number lookup.
Format: ( (buf-tick point-min point-max) last-start-pos line-start line-max)")

(defun mlscroll-line-numbers (&optional win)
  "Calculate and return line numbers.
Returns a list of 3 line numbers at: window start, window end,
and `point-max'.  Uses caching for speed.  If WIN is passed, use the
window limits and `point-max' of the buffer in that window."
  (with-current-buffer (window-buffer win)
    ;(cl-incf (aref mlscroll-cache-stats 0))
    (let* ((mod (car mlscroll-linenum-cache))
	   (last-bt (car mod)) (last-pmn (nth 1 mod)) (last-pmx (nth 2 mod))
	   (cache (cdr mlscroll-linenum-cache))
	   (old-start (car cache))
	   (old-line-start (nth 1 cache))
	   (old-line-max (nth 2 cache))
	   (wstart (window-start win)) (wend (window-end win t))
	   (pmn (point-min)) (pmx (point-max))
	   (buf-tick (buffer-modified-tick))
	   lstart lend lmax)
      (if (and (= buf-tick last-bt) (= pmn last-pmn) (= pmx last-pmx))
	  (setq lstart (if (= wstart old-start) old-line-start
			 (if (< (abs (- wstart old-start)) (- wstart pmn))
			     (funcall (if (> wstart old-start) #'+ #'-)
				      old-line-start
				      (count-lines wstart old-start))
			   ;(cl-incf (aref mlscroll-cache-stats 1))
			   (line-number-at-pos wstart)))
		lend (+ lstart (count-lines wstart wend))
		lmax old-line-max)
	;(cl-incf (aref mlscroll-cache-stats 2))
	(setq mod (list buf-tick pmn pmx))
	(setq lstart (line-number-at-pos wstart)
	      lend   (+ lstart (count-lines wstart wend))
	      lmax   (+ lend   (count-lines wend pmx))))
      (setq mlscroll-linenum-cache (list mod wstart lstart lmax))
      (list lstart lend lmax))))

(defun mlscroll-scroll-to (x &optional idx win)
  "Scroll to the position identified by position X and component IDX.
IDX, if set, is an integer -- 1, 2, or 3 -- identifying which
component of the scrollbar we are in (1=left, 2=current,
3=right).  If IDX is non-nil, X is a pixel position within that
component, starting from 0 at left, _including_ border (for idx=1
only).  If IDX is nil, X can be either a symbol or an integer.
If it's the symbol 'up or 'down, the window is scrolled up or
down by half its height.  Otherwise, X is interpreted as a pixel
position within the entire scrollbar (_not_ including borders).
If WIN is set, it is a window whose buffer to scroll.  Returns
the absolute x position within the full bar (with border width
removed)."
  (pcase-let* ((`(,left ,cur ,right ,start ,end ,last)
		(mlscroll--part-widths win))
	       (barwidth (+ left cur right))
	       (xpos (cond ((symbolp x) ; scroll wheel
			    (+ left (if (eq x 'down)
					(- (/ (float cur) 2))
				      (/ (* (float cur) 3) 2))))
			   ((null idx) x)
			   ((= idx 1) (- x mlscroll-border))
			   ((= idx 2) (+ x left))
			   ((= idx 3) (+ x left cur))
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

(defun mlscroll-mouse (start-event)
  "Handle click and drag mouse events on the mode line scroll bar.
START-EVENT is the automatically passed mouse event."
  (interactive "e")
  (let* ((start-posn (event-start start-event))
	 (start-win (posn-window start-posn))
	 (lcr (cdr-safe (posn-object start-posn)))
	 (x (car (posn-object-x-y start-posn)))
	 (xstart-abs (car (posn-x-y start-posn)))
	 (mouse-fine-grained-tracking t)
	 (xstart (mlscroll-scroll-to x lcr start-win))
	 event end xnew)
    (track-mouse
      (setq track-mouse 'dragging)
      (while (and (setq event (read-event))
		  (mouse-movement-p event))
	(setq end (event-end event)
	      xnew (+ xstart (- (car (posn-x-y end)) xstart-abs)))
	(when (and
	       (eq (posn-area end) 'mode-line)
	       (>= xnew 0)
	       (<= xnew (- mlscroll-width mlscroll-border)))
	  (mlscroll-scroll-to xnew nil start-win))))))

(defun mlscroll--part-widths (&optional win)
  "Pixel widths of the bars (not including border).
Also returns line numbers at window start & end and (point-max).
If optional argument WIN is passed, it should be a window in
which to evaluate the line positions."
  (let* ((lines (mlscroll-line-numbers win))
	 (start (car lines))
	 (end (nth 1 lines))
	 (last (nth 2 lines))
	 (w (- mlscroll-width (* 2 mlscroll-border)))
	 (cur (max mlscroll-minimum-current-width
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
  `(keymap
    (keymap (mode-line keymap
		       (down-mouse-1 . mlscroll-mouse)
		       (wheel-left . ignore)
		       (wheel-right . ignore)
		       (,mouse-wheel-up-event . mlscroll-wheel)
		       (,mouse-wheel-down-event . mlscroll-wheel)))
    help-echo "mouse-1: scroll buffer"))


(defun mlscroll-mode-line ()
  "Generate text for mode line scrollbar.
Intended to be set in an :eval in the mode line, e.g. (as is done
by default if `mlscroll-right-align' is non-nil), in
`mode-line-end-spaces'."
  (pcase-let* ((`(,left ,cur ,right) (mlscroll--part-widths))
	       (bar (concat
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display
				 `(space :width (,(+ left mlscroll-border))))
		     (propertize " " 'face mlscroll-cur-face-properties
				 'display
				 `(space :width (,cur)))
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display
				 `(space :width (,(+ right mlscroll-border)))))))
    (add-text-properties 0 (length bar) mlscroll-extra-properties bar)
    (if mlscroll-right-align
	(concat
	 (propertize " " 'display ; spacer -- align right
		     `(space :align-to (- (+ right right-margin)
					  (,(- mlscroll-width mlscroll-border)))))
	  bar)
      bar)))

(defvar mlscroll-shortfun-mlparts nil
  "Separate parts of the mode line for use when function shortening is enabled.")
(defvar mlscroll-shortfun-remain nil)
(defun mlscroll-shortfun-modeline ()
  (let* ((first (format-mode-line (car mlscroll-shortfun-mlparts)))
	 (cur-length (length first))
	 (mlscroll-shortfun-remain
	  (max mlscroll-shortfun-min-width
	       (- (/ (window-width nil t)
		     mlscroll-mode-line-font-width)
		  cur-length
		  mlscroll-width-chars 3))))
    (format-mode-line `(,first ,@(cadr mlscroll-shortfun-mlparts)))))

(defvar mlscroll-shortfun-saved nil)
(defun mlscroll-shortfun-unsetup ()
  (when mlscroll-shortfun-saved
    (setq mode-line-format (car mlscroll-shortfun-saved))
    (setcdr which-func-current (cdr mlscroll-shortfun-saved))))

(defun mlscroll-shortfun-setup ()
  (if mlscroll-shortfun-min-width
      (let ((mlmi-pos (seq-position mode-line-format 'mode-line-misc-info)))
	(if (not mlmi-pos)
	    (error "mode-line-misc-info not found in the mode-line-format.")
	  (setq mlscroll-shortfun-saved (cons mode-line-format
					      (cdr which-func-current))
		mlscroll-shortfun-mlparts (seq-partition mode-line-format
							 mlmi-pos)
		mode-line-format '(:eval (mlscroll-shortfun-modeline)))
	  (setcdr which-func-current 
		  `((let ((wfc ,(cadr which-func-current)))
		      (if (and wfc (> (length wfc) mlscroll-shortfun-remain))
			  (concat "…" (substring wfc
						 (- 1 mlscroll-shortfun-remain)))
			wfc))))))))

(defvar mlscroll-saved nil)
;;;###autoload
(define-minor-mode mlscroll-mode
  "Minor mode for displaying scroll indicator in mode line."
  :global t
  (if mlscroll-mode
      (progn
	(setq mlscroll-saved mode-line-end-spaces
	      ;; For box to enclose all 3 segments (no internal
	      ;; borders) , they must have the same :foreground (after
	      ;; inversion)
	      mlscroll-flank-face-properties
	      `(:foreground ,mlscroll-out-color
		:box (:line-width ,mlscroll-border)
		:inverse-video t)
	      mlscroll-cur-face-properties
	      `(:foreground ,mlscroll-in-color
	        :box (:line-width ,mlscroll-border)
		:inverse-video t)
	      mlscroll-mode-line-font-width
	      (if (display-multi-font-p)
		  (aref (font-info (face-font 'mode-line)) 11)
		(default-font-width))
	      mlscroll-width
	      (* mlscroll-mode-line-font-width mlscroll-width-chars)
	      line-number-display-limit-width 2000000)
	(if mlscroll-right-align
	    (setq mode-line-end-spaces '(:eval (mlscroll-mode-line))))
	(when (and mlscroll-disable-percent-position
		   (eq (cadar mode-line-position) 'mode-line-percent-position))
	  (setq mlscroll-saved (cons mlscroll-saved (car mode-line-position))
		mode-line-position (cdr mode-line-position)))
	(if mlscroll-shortfun-min-width (mlscroll-shortfun-setup)))
    (mlscroll-shortfun-unsetup)
    (if mlscroll-right-align
	(setq mode-line-end-spaces (car mlscroll-saved)))
    (if (cdr mlscroll-saved)
	(setq mode-line-position (cons (cdr mlscroll-saved)
				       mode-line-position)))))

(provide 'mlscroll)
;;; mlscroll.el ends here
