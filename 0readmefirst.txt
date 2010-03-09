Text following the line of asterisks is of historical note. :-), I'm now
starting to (slowly) code in newlisp.

Please contact me at:
tim@johnsons-web.com

If there are any problems. I recommend that you also post the questions,
comments or caveats at  newLISP Fan Club Forum Index  -> newLISP newS, but
since I don't always check in at the Forum, shoot me an email also.  This has
not been thoroughly tested, any bugs that you find and report or fix will
improve the product. Furthermore there is the possibility that I may have
mistakenly inserted function calls from other elisp modules that I've written,
if you find references to them, I'll get the code imported into the mode.

On-demand documentation has been removed. Too much trouble with parsing.
This has *only* been tested on linux. I now longer use Xemacs, any references
in the code to Xemacs should be considered incidental.

Manifest:
  This file
  tj-parenface.el
  newlisp.el
  dmode.el
  000contrib.txt 
********************************************************************************
Here it is warts and all.

The better part is that now newlisp-mode is a bona-fide major mode, 'inherited'
from scheme-mode, using the 'derived-mode' approach, whereas the original was a
crude hack of another mode. You should find the main source file easier to 
modify.

First, a general disclaimer. The General Public License applies. Nothing is
guaranteed to work.  I assume no liability whatsoever.
This has been tested with both Xemacs and emacs on Linux. I don't use Windows.
You may find differences with Windows. Let me know.

Of practical concern: I am not a newlisp programmer. I worked thru jsmall's
tutorial about a year ago. I hope to start 'studying' newlisp sometime in the
future. But first, I wanted a mode to code with. In the meantime, the elisp
code that I wrote is to a great deal re-usable for other modes, and this should
prove to be a good exercise in customizing other modes to my own tastes.  
(I'm currently coding in rebol, python, javascript, and elisp)

Some things to bear in mind: 
1)Evaluating a function sometimes throws errors. It may be because newlines 
  are not being properly handled.
2)The following strings: "(define", "(defun", "(fn" are search targets for
  marking  a function beginning. Placing a space after the "(" will cause a
  search to fail.
3)Many functions are 'borrowed' directly from scheme-mode. scheme-mode attempts
  to make "the right decision" for you, and sometimes results may not be what
  you expect.
4)Multi-line strings: I used the 'simple' approach for syntax highlighting. 
  If the results are anything like rebol, expect the following: If you edit a 
  multi-line string, the highlight will fail. When finished, move the cursor
  outside of the string and refontify. (This feature is available in the menu).
  There is a more complex approach, such as used for perl-mode that I hope
  to implement in the future.
5)Keymapping: The 'Gods of Emacs' expect the following convention: 
   Mode-specific key sequences should begin with Control-C. 
   The next keypress should be either a Control-alpha combination or
     an non-alpha character.
   Examples: C-c C-a ;; GOOD
             C-c a   ;; BAD
             C-c =   ;; GOOD
   As for my private use, I'll probably unset C-l and use that as a prefix.
   It's easier on my hands. Feel free to remap as you see fit.
   To get a quick handle on commands, when in newlisp-mode, press
   Control-C Control-H (assuming the standard C-c prefix). All commands
   in the menu are keymapped, and you will get a help screen.
6)Syntax highlighting: See "newlisp-user-keywords-regexp" in the main file.
  It's there for you to add your own keywords from your own library.
7)Files:
  a)newlisp.el. Main file. The source of the mode.
  b)tj-parenface.el. To highlight parens and brackets
  c)nl-docs.el. One-liner documentation
    NOTE: Uses associative list.
  d)nl-docstrings. Verbose documentation.
    NOTE: The docs here are produced by docstrings belonging to 
          dummy functions.  I have it in the back of my mind to 
          someday make other uses of those dummy functions.
8)Autoloading:
  ------------
  In your .emacs or .init.el
  (add-to-list 'auto-mode-alist '("\\.lsp\\'" . newlisp-mode))
  (autoload 'newlisp-mode "newlisp" "Turn on NewLisp mode" t)
  (turn-on-font-lock)
9)Upon autoload you should see 2 menus:
  a)"Newlisp" - all functions
  b)"Index" - imenu function indexing
     Turn off 'auto-rescan if you don't want it. 
     Change sort mode if you like. See comments in newlisp.el
  SPECIAL NOTE: See the keymapping for 'forward-sexp in newlisp.el
    You will see that emacs displays it as 'C-c ESC' in the view buffer
    and in the Menu. That is because emacs maps C-[ to ESC.
10)Revisit my reference to windows.
11)Let me know if the translation of the documentation is correct.
   There is a possibility that escaping may have obfuscated some
   text.
12)Let me know of any problems, wish lists or whatever. I hope to make this
   highly functional (eventually) and I hope that the code is written in such
   a way as to make it understandable to a newlisp programmer. 
   Hint! Hint! I can use all the help I can get. :-)
