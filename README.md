# MLScroll
A lightweight scrollbar for the Emacs mode line. 

For graphical window systems:

![mlscroll](https://user-images.githubusercontent.com/93749/116825204-38031880-ab5c-11eb-8252-5f60a61f45dd.gif)

and in terminals:

![mlscroll-terminal](https://user-images.githubusercontent.com/93749/116926527-7239ec00-ac28-11eb-910c-91daaf492284.gif)


# Why

I really only used scrollbars as a visual indication of position and file size.  Recently, to save space, I added `(scroll-bar-mode -1)` to my init, but missed having that information at a glance.  A percentage in the mode line (like `25%`) is not very glanceable, and gives no information about the total line length.  I wanted something very fast and very light weight, plus I'm not so into [rainbow cats](https://github.com/TeMPOraL/nyan-mode).

# Info

- Uses _specified space_ for drawing (only 3 variable-width spaces, actually) for lightning-fast text-based mode line scroll bar.
- Computes line numbers the same way the mode line itself does (fast and [cached](https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers/64656#64656)).
- Works in terminals! Terminal graphics are more granular (the minimum "pixel" is a character wide).  You might want to increase `mlscroll-width-chars` there. 
- In graphical Emacs, you can click, click + drag, and scroll a full window height at a time on the mode line scroll bar.
- Clicking on an inactive window's mode line scroll bar does not activate it. 
- By default, mlscroll disables the XX% position mode line indication.
- MLScroll puts itself in `mode-line-end-spaces`, inserting a spacer to right align.  
- Use `M-x customize-group mlscroll` to change background colors, overall scroll bar width, minimum current "thumb" width, border size, and other settings.

# Lines vs. Characters

The MLScroll bar widths are based on the number of _lines_ shown in the window (+ lines before and after it). The normal scroll bar is based on _characters_.  Both tend to change as very long/wrapped lines come into view, but in the opposite sense: MLScroll sees fewer lines shown and shrinks the current thumb; the normal scrollbar sees many characters in view, and increases it.  I find lines to be more sensible, and it has the advantage that with `truncate-lines` on it doesn't change as you scroll.  If you'd like to see the difference, evaluate `(insert (make-string 5000 ?a) "\n")` in the `*scratch*` buffer amidst other normal text, and scroll through before and after `toggle-truncate-lines`.

# Installing

Get it wherever you get your favorite packages, and arrange to have 

```elisp
(mlscroll-mode 1)
```

called at init time (or whenver you are feeling scrolly). Toggle on or off anytime.
