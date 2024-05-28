# MLScroll - Modeline Scrollbar for Emacs

A lightweight scrollbar for the Emacs mode line. 

For graphical window systems:

&nbsp;&nbsp;&nbsp;&nbsp;![mlscroll](https://user-images.githubusercontent.com/93749/116825204-38031880-ab5c-11eb-8252-5f60a61f45dd.gif)

and terminals too:

&nbsp;&nbsp;&nbsp;&nbsp;![mlscroll-terminal](https://user-images.githubusercontent.com/93749/116926527-7239ec00-ac28-11eb-910c-91daaf492284.gif)

## Why?

Emacs has so many great ways to navigate, I really only ever used scroll-bars as a visual indication of position and file size.  But recently, to save space, I added `(scroll-bar-mode -1)` to my init.  Immediately I missed having that information at a glance.  A percentage in the mode line (like `25%`) is not very glanceable, and also gives no information about the currently visible content relative to the total line length (i.e. from the _length_ of the bar).  I wanted something very fast and very lightweight, plus I'm not so into [rainbow cats](https://github.com/TeMPOraL/nyan-mode).  Hence MLScroll.

## Info

- Uses [_specified space_](https://www.gnu.org/software/emacs/manual/html_node/elisp/Specified-Space.html) for drawing (only 3 variable-width spaces, actually) for lightning-fast text-based mode line scroll.
- The bar lengths convey the number of _lines_ above/visible/below window. Computes line numbers with caching for performance.
- Works in terminals! Terminal graphics are more granular (the minimum "pixel" is a character wide).  You might want to increase `mlscroll-width-chars` there. 
- In graphical Emacs, you can interact with the mode line scroll bar — click, click + drag, and wheel-scroll a full window height at a time.
- Clicking on an inactive window's mode line scroll bar scrolls it, but does _not_ activate that window. 

## Installing

Get it from MELPA, and arrange to have:

```elisp
(mlscroll-mode 1)
```

called at init time (or whenever you are feeling scrolly). Toggle on or off anytime. 

Example for `use-package`:

```elisp
(use-package mlscroll
  :ensure t
  :config
  (setq mlscroll-shortfun-min-width 11) ; truncate which-func
  (mlscroll-mode 1))
```

Alternatively, if you start emacs using the command line `--daemon` command (see below):

```elisp
(use-package mlscroll
   :ensure t
   :hook (server-after-make-frame . mlscroll-mode))
```

## Setup

### Customization

Use `M-x customize-group mlscroll` to change background colors, overall scroll bar width, minimum current "thumb" width, border size, and other settings.

### Position

By default, MLScroll disables the `XX%` position mode line indication, and puts itself in the mode line variable `mode-line-end-spaces`, prepending a spacer to right-align itself.  Optionally, it can instead replace the `XX%` indicator (or be placed anywhere in your mode line).  See `mlscroll-alter-percent-position` and `mlscroll-right-align`.

### Which-function mode 

The builtin `which-function-mode` by default puts `[full-function-name]` on the right side of the mode line.  With long names and the default placement, this can push the MLScroll bar partially or fully off the mode line.  Set `mlscroll-shortfun-min-width` to a minimum width, and MLScroll will truncate the `which-function` name to at least that many trailing characters (e.g. `[…function-name]` or similar), to make room for the scrollbar.  The amount of truncation can be adjusted using `mlscroll-shortfun-extra-width`. 

### Integrating with the mode-line 

Note that MLScroll is most visually compatible with "plain" mode line formats that don't use `:box` bordering.  It will warn you if you try to use a border with a `:box`-full format enabled.  It also doesn't inherit `:underline` and `:overline` mode line properties unless `mlscroll-border` is set to 0 (these don't work with the combination of specified space and `:box`).

See the suggestions for configuring [moody](https://github.com/tarsius/moody) for some config ideas. 

For users of `modus-themes`, `(setq modus-themes-mode-line '(moody borderless))` is recommended, or, in more recent versions (>v4):

```elisp
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))
```

## Scroll-bar width: Lines vs. Characters

The MLScroll bar widths are based on the number of _lines_ visible in the window (+ lines before and after it). The normal scroll bar is based on _characters_ shown. Both tend to change as very long/wrapped lines come into view, but in the opposite sense: MLScroll sees fewer lines shown and shrinks the current thumb; the default scrollbar sees many characters come in view, and grows it.  I find the lines approach to be more sensible, and it has the advantage that with `truncate-lines` on, the thumb doesn't change size as you scroll.  If you'd like to see the difference, evaluate `(insert (make-string 5000 ?a) "\n")` in the `*scratch*` buffer amidst other normal text, and scroll through before and after `toggle-truncate-lines`.  On the other hand, highly folded documents like org-mode docs will show a changing "thumb size" as you scroll through, as the current window could contain _many_ (hidden) lines. I find that pretty convenient actually.

## FAQ's

- **How does it work?** MLScroll places itself by default in `mode-line-end-spaces`, and uses a right-aligned space to align it at the end of the modeline.  For drawing the bars themselves, it uses three "specified spaces".

- **MLScroll doesn't work with my fancy mode line mode!** It should work automatically for simple mode lines that end in `mode-line-end-spaces`.  If you prefer,  set `mlscroll-right-align` to `nil` and `mlscroll-alter-percent-position` to `'replace` to put it in place of the `XX%` percentage indicator.  Otherwise, e.g. if you have a highly customized or pre-packaged mode line, you'll need to find somewhere to put MLScroll.  A general recipe is to:
	1. Set `mlscroll-right-align` to `nil`.
	2. Set the mode line scroller into the relevant mode-line variable directly yourself, like so: `(setq fancy-mode-line-variable-of-some-kind '(:eval (mlscroll-mode-line))`. 
	3. Alternatively, if you didn't design your mode line yourself or find this too complicated, ask whoever did to support MLScroll. 

- **MLScroll starts off very small when I start an emacs session using `--daemon`.** For graphical windows, MLScroll needs to know the width of font characters in the mode line (or at least the default font) to draw a pixel-perfect bar. Since `--daemon` doesn't create a frame or know anything about the font widths, loading MLScroll directly under a `--daemon` session misreports the font width as 1 pixel, leading to a very small scroller bar.  The solution is either to abandon `--daemon` in favor of [`(server-start)`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html) in your init file, or arrange for MLScroll to be initialized _later_, after a frame is created, ala:
  ```elisp
  (use-package mlscroll
    :hook (server-after-make-frame . mlscroll-mode))
  ```
- **How can I customize MLScroll?** `M-x customize-group mlscroll [Ret]`. 

- **I want to use MLScroll with different themes throughout the day, what should I do?** This should happen automatically since v0.2.  Since the MLScroll "thumb" color defaults to the foreground color of the `scroll-bar` face, you might configure that face for your theme, rather than `mlscroll-in-color` directly.

- **I get a message about :box disabling my MLScroll border**: MLScroll uses the `:box` attribute to draw _border_ (with the help of `:inverse-video`).  If your normal mode line face already has a `:box` property, this will interfere and cause the left/right border to show up.  If you want a border to make your MLScroll less tall, consider disabling the `:box` property on faces `'mode-line` and `'mode-line-inactive` (`M-x customized-group mode-line-faces [Ret]`).  See [issue](https://github.com/jdtsmith/mlscroll/issues/3).  For users of `modus-themes`, see above.

## Other tips

MLScroll takes up a decent (configurable) chunk of your mode line.  To save space for it even when the window is somewhat narrow, I use:

- [minions](https://github.com/tarsius/minions) to hide all minor-modes under a nice menu.
- [cyphejor](https://github.com/mrkkrp/cyphejor) to shorten the names of major mode using single-char emoji and greek characters.
- `(setq mlscroll-shortfun-min-width 11)` to trim down the which-function name as needed. 
- removal of mule-info, and a trim of all double-spaces anywhere in the mode line format to a single space:
```elisp
  (setq-default
   mode-line-format ;less space, no MULE
   (cl-nsubst-if " " (lambda (x) (and (stringp x) (string-blank-p x) (> (length x) 1)))
		 (remove 'mode-line-mule-info mode-line-format))))
```
