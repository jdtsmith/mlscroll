# MLScroll
Lightweight scrollbar for the Emacs mode line

![mlscroll](https://user-images.githubusercontent.com/93749/116825204-38031880-ab5c-11eb-8252-5f60a61f45dd.gif)

# Why

I really only used scrollbars for a visual indication of position and file length.  Recently, to save space, I added `(scroll-bar-mode -1)` to my init, but missed having that information.  A percentage in the mode line (like `25%`) is not very glaceable, and gives no information about the total line length.  I wanted something very fast and very light weight, plus I'm not so into [rainbow cats](https://github.com/TeMPOraL/nyan-mode).  

# Info

- Uses _specified space_ (only 3 variable-width spaces, actually) for lightning-fast text-based mode line scroll bar.
- Computes line numbers the same way the mode line itself does (fast and [cached](https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers/64656#64656)).
- You can click, click + drag, and scroll a full window height at a time on the mode line scroll bar.
- Clicking on an inactive window's mode line scroll bar does not activate it. 
- By default, mlscroll disables the XX% position mode line indication/
- MLScroll puts itself in `mode-line-end-spaces`, inserting a spacer to right align.  
- Use `M-x customize-group mlscroll` to change background colors, overall scroll bar width, minimum current "thumb" width, and border size.

# Installing

Get it wherever you get your favorite packages, and arrange to have 

```elisp
(mlscroll-mode 1)
```

called at init time (or whenver you are feeling scrolly). Toggle on or off anytime.
