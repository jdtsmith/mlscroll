# MLScroll
A lightweight scrollbar for the Emacs mode line. 

For graphical window systems:

![mlscroll](https://user-images.githubusercontent.com/93749/116825204-38031880-ab5c-11eb-8252-5f60a61f45dd.gif)

and in terminals:

![mlscroll-terminal](https://user-images.githubusercontent.com/93749/116926527-7239ec00-ac28-11eb-910c-91daaf492284.gif)


# Why

Emacs has so many great ways to navigate, I really only ever used scrollbars as a visual indication of position and file size.  But recently, to save space, I added `(scroll-bar-mode -1)` to my init.  Immediately I missed having that information at a glance.  A percentage in the mode line (like `25%`) is not very glanceable, and also gives no information about the total line length.  I wanted something very fast and very light weight, plus I'm not so into [rainbow cats](https://github.com/TeMPOraL/nyan-mode).

# Info

- Uses [_specified space_](https://www.gnu.org/software/emacs/manual/html_node/elisp/Specified-Space.html) for drawing (only 3 variable-width spaces, actually) for lightning-fast text-based mode line scroll.
- Computes line numbers with caching for performance.
- Works in terminals! Terminal graphics are more granular (the minimum "pixel" is a character wide).  You might want to increase `mlscroll-width-chars` there. 
- In graphical Emacs, you can click, click + drag, and wheel-scroll a full window height at a time on the mode line scroll bar.
- Clicking on an inactive window's mode line scroll bar does not activate it. 
- By default, MLScroll disables the `XX%` position mode line indication.
- MLScroll puts itself in the mode line variable `mode-line-end-spaces`, prepending a spacer to right align itself.
- Use `M-x customize-group mlscroll` to change background colors, overall scroll bar width, minimum current "thumb" width, border size, and other settings.
- `which-function-mode` by default puts `[full-function-name]` on the right of the mode line.  With long names this can push the scroll bar off the right side of the mode line.  Set `mlscroll-shortfun-min-width` to a minimum width, and MLScroll will truncate the function name to at least that many trailing characters (`[…function-name]` or similar). 

# Lines vs. Characters

The MLScroll bar widths are based on the number of _lines_ visible in the window (+ lines before and after it). The normal scroll bar is based on _characters_ shown. Both tend to change as very long/wrapped lines come into view, but in the opposite sense: MLScroll sees fewer lines shown and shrinks the current thumb; the default scrollbar sees many characters come in view, and grows it.  I find the lines approach to be more sensible, and it has the advantage that with `truncate-lines` on, the thumb doesn't change size as you scroll.  If you'd like to see the difference, evaluate `(insert (make-string 5000 ?a) "\n")` in the `*scratch*` buffer amidst other normal text, and scroll through before and after `toggle-truncate-lines`.

# Installing

Get it from MELPA, and arrange to have

```elisp
(mlscroll-mode 1)
```

called at init time (or whenever you are feeling scrolly). Toggle on or off anytime. Example for `use-package`:

```elisp
(use-package mlscroll
  :ensure t
  :config
  (setq mlscroll-shortfun-min-width 11) ;truncate which-func, for default mode-line-format's
  (mlscroll-mode 1))
```
# FAQ's

- **How does it work?** MLScroll places itself in `mode-line-end-spaces`, and uses a right-aligned space to align it at the end of the modeline.

- **MLScroll doesn't work with my fancy mode line mode!** It should work automatically for simple mode lines that end in `mode-line-end-spaces`.  If you have a highly customized or pre-packaged mode line, you'll need to find somewhere to put MLScroll.  A general recipe is to:
	1. Set `mlscroll-right-align` to `nil`.
	2. Set the mode line scroller into the relevant mode-line variable directly yourself, like so: `(setq fancy-mode-line-variable-of-some-kind '(:eval (mlscroll-mode-line))`. 
	3. Alternatively, if you didn't design your mode line yourself, ask whoever did to support MLScroll. 

- **I'm getting errors about mode-line-misc-info:** You probably have `mlscroll-shortfun-min-width` set to non-`nil` with a non-standard mode line format.  Function name shortening relies on dividing the mode line at `mode-line-misc-info` and computing how much space it takes, then altering `which-func-format` (inside of `mode-line-misc-info`) to truncate the function name appropriately. 

# Other tips

MLScroll takes up a decent (configurable) chunk of your mode line.  To save space for it even when the window is somewhat narrow, I use:

- [minions](https://github.com/tarsius/minions) to hide all minor-modes under a nice menu. 
- [cyphejor](https://github.com/mrkkrp/cyphejor) to shorten the names of major mode using emoji and greek characters. 
- `(setq mlscroll-shortfun-min-width 11)` to trim down the which-function name as needed. 
- removal of mule-info, and a trim of all double-spaces anywhere in the mode line format to a single space:
```elisp
  (setq-default
   mode-line-format ;less space, no MULE
   (cl-nsubst-if " " (lambda (x) (and (stringp x) (string-blank-p x) (> (length x) 1)))
		 (remove 'mode-line-mule-info mode-line-format))))
```
