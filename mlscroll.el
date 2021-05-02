;;; mlscroll.el --- A scroll bar for the modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/mlscroll
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.0.1
;; Keywords: mode line scrolling

;;; Commentary:

;; MLScroll provides a small and lean graphical (text based) scrollbar for the
;; mode line.  To use, simply load the package and add:
;;
;;   (mlscroll-mode 1)
;;
;; to your configuration

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
If nil, you must arrange to include '(eval: (mlscroll-mode-line))
somewhere in `mode-line-format'."
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

(defcustom mlscroll-width (* (default-font-width) 12)
  "Width in pixels of the mode line scroll indicator.
Default is 12 characters wide."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-minimum-current-width 2
  "Minimum pixel width of the current window region (central) bar."
  :group 'mlscroll
  :type 'integer)

(defcustom mlscroll-border (floor (/ (float (default-font-height)) 4))
  "Border in pixels around the scrollbar.
Drawn in the mode line's background color. Defaults to 1/4 of the
default font's character height."
  :group 'mlscroll
  :type 'integer)

(defun mlscroll-fast-line-number-at-pos (pos &optional win)
  "Line number at position.
Compute line number at position POS. Uses fast mode-line
formatting.  If WIN is non-nil, find line number at position within
that window."
  (let ((old (window-point win)))
    (set-window-point win pos)
    (prog1
	(string-to-number (format-mode-line "%l" 0 win))
      (set-window-point win old))))

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
  (pcase-let* ((`(,left ,cur ,right ,start ,end ,last ,wstart) (mlscroll-part-widths win))
	       (barwidth (+ left cur right))
	       (xpos (cond ((symbolp x) ; scroll wheel
			    (+ left (if (eq x 'down) (- cur) (/ (* (float cur) 3) 2))))
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
	   (goto-char wstart)
	   (forward-line (- targ start))
	   (point)))))
    xpos))

(defun mlscroll-wheel (event)
  (interactive "e")
  (let ((type (event-basic-type event))
	(win (posn-window (event-start event))))
    (mlscroll-scroll-to
     (if (eq type mouse-wheel-up-event) 'up 'down)
     nil win)))

(defun mlscroll-mouse (start-event)
  "Handle click and drag mouse events on the mode line scroll bar."
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

(defun mlscroll-part-widths (&optional win)
  "Pixel widths of the bars (not including border).
If optional argument WIN is passed, it should be a window in
which to evaluate the line positions."
  (let* ((wstart (window-start win))
	 (start (mlscroll-fast-line-number-at-pos wstart win))
	 (w (- mlscroll-width (* 2 mlscroll-border)))
	 (wh (window-body-height win))
	 last end cur left right)
    (with-current-buffer (window-buffer win)
      (setq end  (+ start (count-lines wstart (window-end win t)))
	    last (mlscroll-fast-line-number-at-pos (point-max) win)))
    (setq cur (max mlscroll-minimum-current-width
		   (round
		    (* w (/ (float (if (< (- last start -1) wh)
				       (- end start -1) ;partially filled
				     (max wh (- end start -1))))
			    last))))
	  left (if (and (= start 1) (= last end)) 0
		 (max 0 (round (* (- w cur)
				  (/ (- start 1.0)
				     (+ (- start 1) (- last end)))))))
	  right (max 0 (- w cur left)))
    (list left cur right start end last wstart)))

(defvar mlscroll-flank-face-properties nil)
(defvar mlscroll-cur-face-properties nil)
(defvar mlscroll-spacer
  (propertize " " 'display ; spacer -- align right
	      `(space :align-to (- (+ right right-margin
				      (,mlscroll-border))
				   (,mlscroll-width))))
  "A specified space which right-aligns the scroll bar")
(defconst mlscroll-extra-properties
  `(keymap
    (keymap (mode-line keymap
		       (down-mouse-1 . mlscroll-mouse)
		       (,mouse-wheel-up-event . mlscroll-wheel)
		       (,mouse-wheel-down-event . mlscroll-wheel)))
    help-echo "mouse-1: scroll buffer"))

(defun mlscroll-mode-line ()        
  "Generate text for mode line scrollbar.
Intended to be set in an :eval in the mode line, e.g. (as is done
by default if `mlscroll-right-align' is non-nil), in
`mode-line-end-spaces'."
  (pcase-let* ((`(,left ,cur ,right) (mlscroll-part-widths))
	       (bar (concat
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display `(space :width (,(+ left mlscroll-border))))
		     (if (> cur 0)
			 (propertize " " 'face mlscroll-cur-face-properties
				     'display `(space :width (,cur))))
		     (propertize " " 'face mlscroll-flank-face-properties
				 'display `(space :width (,(+ right mlscroll-border)))))))
    (add-text-properties 0 (length bar) mlscroll-extra-properties bar)
    (if mlscroll-right-align (concat mlscroll-spacer bar) bar)))
	      
(defvar mlscroll-saved nil)
(define-minor-mode mlscroll-mode 
  "Minor mode for displaying scroll indicator in mode line."
  :global t
  (if mlscroll-mode
      (progn
	(if mlscroll-right-align
	    (setq mode-line-end-spaces '(:eval (mlscroll-mode-line))))
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
	      line-number-display-limit-width 2000000)
	(when (and mlscroll-disable-percent-position
		   (eq (cadar mode-line-position) 'mode-line-percent-position))
	  (setq mlscroll-saved (cons mlscroll-saved (car mode-line-position))
		mode-line-position (cdr mode-line-position))))
    (if mlscroll-right-align
	(setq mode-line-end-spaces (car mlscroll-saved)))
    (if (cdr mlscroll-saved)
	(setq mode-line-position (cons (cdr mlscroll-saved)
				       mode-line-position)))))

(provide 'mlscroll-mode)
