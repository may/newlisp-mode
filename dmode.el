;;; package --- Summary
;;  dmode.el --- a guide for implementing a derived mode.
;;    - should be of particular interest to one who wishes to extend
;;      a programming mode.
;;  this file is not a part of gnu Emacs or Xemacs
;;  Author: tim johnson <tim@johnsons-web.com> (TJ)
;;
;;; Commentary:
;;    Thanks to Stefan Monnier <monnier@iro.umontreal.ca> (SM)
;;    Thanks also to: johan bockgård <bojohan+news@dd.chalmers.se> (JB)
;;
;;; Related links and files
;;    http://www.emacswiki.org/cgi-bin/wiki/DerivedMode,
;;    http://www.iro.umontreal.ca/~monnier/elisp/bibtex-style.el
;;    http://www.emacswiki.org/cgi-bin/wiki?SampleMode
;;    derived.el, which should be included with your Emacs or Xemacs installation
;;
;; 'Emacs' is meant to refer to *either* GNU Emacs *or* to the Xemacs fork
;;
;;  About 'help-command': The standard Emacs installation maps control-h to the
;;    'help-command' prefix.  Sometimes control-h is mapped to backward-delete.
;;      if you have done so, then where 'c-h' is used in this file, substitute
;;      the appropriate prefix (such as F1)
;;
;; Derived mode is an Emacs feature that allows you to 'inherit' a major mode as a 'parent',
;;    and create a new major mode with the 'parent' features and add new features.
;; 'define-derived-mode is defined in derived.el as a macro form.
;; 
;;; History:
;;    version 0.1
;; 
;;; Why implement 'derived-mode?
;;  1)As a programmer, you might wish to enable additional syntax highlighting, such as keywords
;;    defining your own libraries or additional packages for the language of your choice.
;;    you may wish to install further functionality, like a template package for python not
;;    included in the original major mode distribution; remap keystrokes, add new keystrokes, etc.
;;  2)You may be working with a language extension, such as the "C" dynace extension or you
;;    might be working with a language that is not included among Emacs major modes and
;;    you wish to "roll your own" - based on and leveraging an existing major mode.
;;
;;  I speak from the viewpoint of a programmer, doubtless there are many, many other uses for
;;  derived-mode.  Example: in the future, I may wish to implement on-demand documentation with some
;;    simple syntax highlighting, so I may wish to use fundamental mode as a 'parent'.
;;
;;; PLEASE NOTE:
;;    This template is a special case.  It is my hope that one may learn about the details
;;    of developing a derived mode here, but many features may be unnecessary and other
;;    features are not included.  For other examples see `Related Links and Files'
;; 
;; The following code is not functional, but a great deal of the "work" of implementing a
;; derived mode may be achieved by following these steps:
;; 1)Where 'parent' is part of a symbol, substitute a symbol that describe the parent that
;;   you wish to 'inherit'.
;;   example: 'parent-mode => 'python-mode
;;            'parent-mode => 'scheme-mode
;;            'parent-font-lock-keywords => 'python-font-lock-keywords
;; 2)Where 'dmode' is part of a symbol, substitute the name of your 'child' mode.
;; 3)Follow further examples where illustrated
;; !)Results will vary.  I will be installing this file at a website and adding notes
;;   as I implement this.
;;
;;; Code:
;; ===========================================================================================
(require 'parent-mode)
;; ===========================================================================================
;; Not all major or derived modes need to define new faces. This example need not be used.
;; This is best used for those who may wish to add new keywords to the mode. Such new
;; keywords might be (as an example) additional standard features of the language OR
;; the programmer's own library routines.
;; Additionally, one may wish to colorize language features not covered in the parent mode.
;;   Some find this irritating, others - myself included - whose eyesite is less than ideal,
;;   might find additional contrast helpful.
;; ===========================================================================================
(defface dmode-font-lock-keywords-face
  '((((class color) (background light)) (:foreground "green4"))
    (((class color) (background dark)) (:foreground "yellow"))
    (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
    (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
    (t (:bold t)))
  "sample font lock mode face used to highlight
   a syntax group for the derived mode."
  :group 'font-lock-faces)
(defvar dmode-font-lock-keywords-face 'dmode-font-lock-keywords-face)
;; ==========================================================================
;; Using 'regexp-opt, construct a regular expression from a list of keywords
;;   that will be highlight with our sample face above.
;; ==========================================================================
(defconst
  dmode-keywords-regexp
   (regexp-opt '(   ;; c-h f regexp-opt <ret>
    ;; list of keywords here
    )))
;; ==========================================================================
(defvar dmode-font-lock-keywords
   `(,@parent-font-lock-keywords  ;; note: backquote and splice operator!
     ;; add new keywords for highlighting in our sample face
     (,(concat "\\<\\(" dmode-keywords-regexp "\\)\\>")  ;; keywords + word boundaries
      0 dmode-font-lock-keywords-face)
     ;; To illustrate the use of a string literal regex:
     ;;   add braces "{}" for multi-line strings to an existing face
     ;;   NOTE: emacs does not handle multi-line string well in this manner.
     ;;     (JB) suggests looking at how perl and AUCTex handle this.
     ("[^#]\\({[^{}]*}\\)" 0 'font-lock-string-face)
     )
   "List of dmode keywords and faces.")
;; ==========================================================================
;; Construct a keymap for the mode.
;; Traditionally, Emacs reserves Control-c for a major mode prefix.
;; However, in case the parent mode has already made extensive use of the
;; Control-c prefix, I illustrate the alternative of Control-=
;; ==========================================================================
(defvar dmode-mode-map
  (let ((map (make-sparse-keymap))) ;; c-h make-sparse-keymap <RET>
	;; Here we may define any number of key sequences for our mode
	;; c-h define-key <RET>
	(define-key map [(control =) (b)] 'dmode-test-fun)
	map)
  "Keymap for `dmode-mode'.")
;; ==========================================================================
;; Just a sample function to test the menu and key definition.
;; ==========================================================================
(defun dmode-test-fun()
  "Use to test keymapping and menu"
  (interactive)
  (message-box "Hello World from dmode-mode."))
;; ==========================================================================
;; Define the menu using 'easy-menu-define for
;; best compatibility for both forks.
;; ==========================================================================
(easy-menu-define    ;; c-h f easy-menu-define <RET>
  ;; symbol----keymap---------documentation
  ;;  |         |              |
  dmode-menu dmode-mode-map "dmode Mode Menu"
  ;; menu:
  '("dmode"                  ;; Title
  ;; Item(s) .....
	;; name, callback
	["Test" dmode-test-fun])
  )
;; ==========================================================================
(define-derived-mode dmode-mode parent-mode "dmode"
  "A major mode for dmode."
  (easy-menu-add dmode-menu)
  ;; Highly Recommended: c-h v font-lock-keywords <RET>
  (set (make-local-variable 'font-lock-defaults)
       (cons 'dmode-font-lock-keywords
             (or (cdr font-lock-defaults)
                 '(nil t ;; syntax table modifications follow: You may wish to use
                     ;; the table from the parent mode, and add to
                    ;; if necessary.
                    ;; For help: C-h f modify-syntax-entry <RET>
                    ;; In this example, we bind non-alpha characters to the 'word' syntax class
                       ((?+ . "w") (?- . "w") (?* . "w") (?/ . "w")
                        (?. . "w") (?< . "w") (?> . "w") (?= . "w")
                        (?? . "w") (?$ . "w") (?% . "w") (?_ . "w")
                        (?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))))))
                        ;; NOTE: Emacs accepts a more compact approach.
                        ;; The cons-cell list approach used here is for XEmacs compatibility.
                        ;; Thanks to (JB)!
                        ;; *AND* remember! This is just an example!
                        ;; ========================================
  )
;;; dmode.el ends here
