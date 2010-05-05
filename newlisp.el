;;; package --- Summary
;;  newlisp.el --- An Emacs mode for newlisp
;;  this file is not a part of gnu Emacs or Xemacs
;;  Author: Tim johnson <tim@johnsons-web.com> (TJ)
;;
;;; License:

;; Copyright (C) 2006 Tim Johnson

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; History:
;;    V 0.25
;;      1)Added functions for sexp-cursor movement:
;;        newlisp-indent-and-move-next, newlisp-indent-and-move-back,
;;        newlisp-prev-opening-parens, newlisp-next-opening-parens,
;;      2)Improved newlisp-next-function, newlisp-previous-function
;;    V 0.20
;;      1)See additions by frontera000
;;      2)newlisp-next-functionp: If cursor is at beginning of function '('
;;        move forward to enable search for next function
;;      3)newlisp-delete-sexp: delete sexp nearest to cursor or from '(' or ')'
;;      4)newlisp-context-qualify: upcase previous word and insert ':'
;;    V 0.10
;;      1)Newlisp documentation is now parsed without preperation
;;      2)One associative list is produced, for both one liners and verbose
;;        documentation. Discarded the use of dummy functions.
;;      3)Added 'newlisp-clear-comint-buffer. If comint buffer is
;;        not open, will also open it.
;;      4)Added newlisp-mode-hook so that you can make your own
;;        individual customizations via 'add-hook
;;    mode. V 0.002
;;    Originally a shameless hack of quack.el. Now a properly derived major
;;; Commentary:
;;    Thanks to Stefan Monnier <monnier@iro.umontreal.ca> (SM)
;;    Thanks also to: johan bockgård <bojohan+news@dd.chalmers.se> (JB)
;;
;;; Related links and files
;;    http://www.johnsons-web.com/demo/emacs/derived-mode/dmode.el
;;      Mode template to "roll your own programming mode"
;; 
;; 'Emacs' is meant to refer to *either* GNU Emacs *or* to the Xemacs fork
;;
;;  About 'help-command': The standard Emacs installation maps control-h to the
;;    'help-command' prefix.  Sometimes control-h is mapped to backward-delete.
;;      if you have done so, then where 'c-h' is used in this file, substitute
;;      the appropriate prefix (such as F1)
;;
;;; Quickstart:
;;     control-c control-h
;;; Code:
;; ===========================================================================================
(require 'scheme)          ;; Inherit Scheme mode
(require 'tj-parenface)    ;; Highlight parens and brackets
;(require 'nl-doc-assoc)    ;; Data structure holding documentation
;; ===========================================================================================
(defvar newlisp-mode-hook nil
  "*Hook called by `newlisp-mode'.")
;; ===========================================================================================
(defvar newlisp-function-begin-regexp "(\\(?:def\\(?:ine\\|un\\)\\|fn\\)"
  "Used to find function definitions. NOTE: No whitespace after parens!")
;; ===========================================================================================
;;  'helper' functions
;; ===========================================================================================
(defun safe-kill-buff (n)
  "kill a buffer, don't worry whether it exists."
  (interactive)
  (condition-case nil
				  (kill-buffer n)
				  (error nil) ) )
;; ===========================================================================================
(defun newlisp-replace-newlines (S)
  "Replace newlines in string 'S' with spaces.
   Use for sending code to newlisp."
  (mapconcat (lambda (x) x) (split-string S "\n") " "))
;; ===========================================================================================
;; 2008-03-12: Jeff Ober: was missing define-macro, lambda, lambda-macro
(defvar newlisp-function-names '("define" "define-macro" "fn" "lambda" "lambda-macro") 
  "Names of Newlisp function definitions")
;; ===========================================================================================
(defun newlisp-at-function-startp ()
  "Is cursor at the beginning of a function?"
  (interactive)
  (cond ((string-equal (char-to-string (char-after)) "(") ;; cursor on '('
		 (forward-char 1)
		 (cond ((member (current-word) newlisp-function-names)
				(backward-char 1) ;; found. Reset
				(message "found")
				t)
			   (t ;; not found. Reset
				(message "not found")
				 (backward-char 1) nil)))
		(t  nil)))
;; ===========================================================================================
(put 'fn 'scheme-indent-function 1)  ;; treat fn,letn,letex as a functions
(put 'letn 'scheme-indent-function 1)  ;; CONTRIB: Rick Hanson
(put 'letex 'scheme-indent-function 1)
(put 'local 'scheme-indent-function 1) ;; CONTRIB: Jeff Ober
(put 'lambda-macro 'scheme-indent-function 1) ;; CONTRIB: Jeff Ober
;; ===========================================================================================
;;  Create some faces for special fontification.
;;  NOTE: XEmacs seems to ignore the (background light) form.
;;  IOWS: You may need to use customize to set a readable color if using light background
;; ===========================================================================================
(defface newlisp-font-lock-keywords-face
  '((((class color) (background light)) (:foreground "green4"))
; Uncomment this and delete the following line to reduce visual color-coding
; clutter - NEM 
;    (((class color) (background dark)) (:foreground "green")) ;; uncomment this
    (((class color) (background dark)) (:foreground "cyan")) ;; delete this
    (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
    (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
    (t (:bold t)))
  "Font lock mode face used to highlight a syntax group for newlisp mode."
  :group 'font-lock-faces)
(defvar newlisp-font-lock-keywords-face 'newlisp-font-lock-keywords-face)
;; ===========================================================================================
(defface newlisp-font-lock-function-names-face
  '((((class color) (background light)) (:foreground "darkcyan"))
    (((class color) (background dark)) (:foreground "cyan"))
    (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
    (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
    (t (:bold t)))
  "Font lock mode face used to highlight functions (defun, define, fn) for newlisp mode."
  :group 'font-lock-faces)
(defvar newlisp-font-lock-function-names-face 'newlisp-font-lock-function-names-face)
;; ===========================================================================================
(defface newlisp-font-lock-user-keywords-face
  '((((class color) (background light)) (:foreground "red4"))
    (((class color) (background dark)) (:foreground "dark slate gray")) ;; NEM was yellow3
    (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
    (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
    (t (:bold t)))
  "Font lock mode face used to highlight user-defined keywords for newlisp mode."
  :group 'font-lock-faces)
(defvar newlisp-font-lock-user-keywords-face 'newlisp-font-lock-user-keywords-face)
;; ===========================================================================================
(defface newlisp-font-lock-quote-face
  '((((class color) (background light)) (:foreground "purple"))
		;; 2008-03-12 Jeff Ober: changed to plum from magenta. I *hate* magenta.
    (((class color) (background dark)) (:foreground "plum"))
    (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
    (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
    (t (:bold t)))
  "Font lock mode face used to highlight quoted symbols in newlisp mode."
  :group 'font-lock-faces)
(defvar newlisp-font-lock-quote-face 'newlisp-font-lock-quote-face)
;; ==========================================================================
(defconst 
  newlisp-function-names-regexp
  (regexp-opt '("define" "fn")))
;; ==========================================================================
;; 2008-03-12 Jeff Ober: updated with 9.3.3's symbols [(map string (symbols))]
(defconst
  newlisp-keywords-regexp
   (regexp-opt '(   ;; c-h f regexp-opt <ret>
		"!" "!=" "$" "$0" "$1" "$10" "$11" "$12" "$13" "$14" "$15" "$2" "$3" "$4" "$5" "$6" 
		 "$7" "$8" "$9" "$args" "$idx" "$main-args" "%" "&" "*" "+" "-" "/" ":" "<" "<<" 
		 "<=" "=" ">" ">=" ">>" "?" "@" "MAIN" "NaN?" "^" "abs" "acos" "acosh" "add" "address" 
		 "amb" "and" "append" "append-file" "apply" "args" "array" "array-list" "array?" 
		 "asin" "asinh" "assoc" "assoc-set" "atan" "atan2" "atanh" "atom?" "base64-dec" "base64-enc" 
		 "bayes-query" "bayes-train" "begin" "beta" "betai" "bind" "binomial" "callback" 
		 "case" "catch" "ceil" "change-dir" "char" "chop" "clean" "close" "command-line" 
		 "cond" "cons" "constant" "context" "context?" "copy-file" "cos" "cosh" "count" "cpymem" 
		 "crc32" "crit-chi2" "crit-z" "current-line" "curry" "date" "date-value" "debug" 
		 "dec" "def-new" "default" "define" "define-macro" "delete" "delete-file" "delete-url" 
		 "destroy" "det" "device" "difference" "directory" "directory?" "div" "do-until" 
		 "do-while" "doargs" "dolist" "dostring" "dotimes" "dotree" "dump" "dup" "empty?" 
		 "encrypt" "ends-with" "env" "erf" "error-event" "error-number" "error-text" "eval" 
		 "eval-string" "exec" "exists" "exit" "exp" "expand" "explode" "factor" "fft" "file-info" 
		 "file?" "filter" "find" "find-all" "first" "flat" "float" "float?" "floor" "flt" 
		 "for" "for-all" "fork" "format" "fv" "gammai" "gammaln" "gcd" "get-char" "get-float" 
		 "get-int" "get-long" "get-string" "get-url" "global" "global?"
                 "if" "if-not" "ifft" "import" 
		 "inc" "index" "int" "integer" "integer?" "intersect" "invert" "irr" "join" "lambda?" 
		 "last" "legal?" "length" "let" "letex" "letn" "list" "list?" "load" "local" "log" 
		 "lookup" "lower-case" "macro?" "main-args" "make-dir" "map" "mat" "match" "max" 
		 "member" "min" "mod" "mul" "multiply" "name" "net-accept" "net-close" "net-connect" 
		 "net-error" "net-eval" "net-listen" "net-local" "net-lookup" "net-peek" "net-peer" 
		 "net-ping" "net-receive" "net-receive-from" "net-receive-udp" "net-select" "net-send" 
		 "net-send-to" "net-send-udp" "net-service" "net-sessions" "new" "nil" "nil?" "normal" 
		 "not" "now" "nper" "npv" "nth" "nth-set" "null?" "number?" "open" "or" "ostype" 
		 "pack" "parse" "parse-date" "peek" "pipe" "pmt" "pop" "pop-assoc" "post-url" "pow" 
		 "pretty-print" "primitive?" "print" "println" "prob-chi2" "prob-z" "process" "protected?" 
		 "push" "put-url" "pv" "quote" "quote?" "rand" "random" "randomize" "read-buffer" 
		 "read-char" "read-file" "read-key" "read-line" "real-path" "ref" "ref-all" "ref-set" 
		 "regex" "remove-dir" "rename-file" "replace" "replace-assoc" "reset" "rest" "reverse" 
		 "rotate" "round" "save" "search" "seed" "seek" "select" "semaphore" "sequence" "series" 
		 "set" "set-assoc" "set-locale" "set-nth" "set-ref" "set-ref-all" "setq" "sgn" "share" 
		 "signal" "silent" "sin" "sinh" "sleep" "slice" "sort" "source" "sqrt" "starts-with" 
		 "string" "string?" "sub" "swap" "sym" "symbol?" "symbols" "sys-error" "sys-info" 
		 "tan" "tanh" "throw" "throw-error" "time" "time-of-day" "timer" "title-case" "trace" 
		 "trace-highlight" "transpose" "trim" "true" "true?" "unicode" "unify" "unique" "unless" 
		 "unpack" "until" "upper-case" "utf8" "utf8len" "uuid" "wait-pid" "when" "while" 
		 "write-buffer" "write-char" "write-file" "write-line" "xml-error" "xml-parse" "xml-type-tags" 
		 "zero?" "|" "~"
    )))
;; ==========================================================================
(defconst
  newlisp-user-keywords-regexp
   (regexp-opt '(   ;; c-h f regexp-opt <ret>
   ;; for your own libraries
   ;"aligned?" "even?" "ltrim" "ltrims" "parseint" "rtrim" "rtrims" "second" "third";; test
    )))
;; ==========================================================================
(defun newlisp-indent-and-move-next ()
  "NOTE: Indentation is done via lisp indentation rules. 
  Not 'default-tab-width."
  (lisp-indent-line)
  (next-line))
;; ==========================================================================
(defun newlisp-indent-and-move-back ()
  (lisp-indent-line)
  (previous-line))
;; ==========================================================================
(defun newlisp-prev-opening-parens ()
  (re-search-backward "("))
;; ==========================================================================
(defun newlisp-next-opening-parens ()
  (if (eq (char-after) 40)
	(forward-char 1))
  (re-search-forward "(")
  (backward-char 1))
;; ==========================================================================
(defun newlisp-sexp-start ()
  "Move point to nearest opening parens"
  (interactive)
  (if 
	(not (eq (char-after) 40)) 
	(re-search-backward "(")))
;; ==========================================================================
(defun newlisp-sexp-end()
  "Move point to nearest closing parens"
  (interactive)
  (re-search-forward ")"))
;; ==========================================================================
;;  Inferior process functions and constants
;; ==========================================================================
(defun newlisp-select-sexp ()
  "Select the innermost sexp (closest to cursor)"
  (interactive)
  (newlisp-sexp-start)
  (set-mark (point))
  (forward-sexp))
;; ==========================================================================
(defun newlisp-select-function ()
  "Select enclosing function OR 
previous function if cursor not inside of a function sexp.
Cursor moved to end of function."
  (interactive)
  (let ((found nil))
		(cond ((newlisp-at-function-startp) (setq found t))
			  	((newlisp-previous-functionp) (setq found t)))
		(cond
			(found
				(set-mark (point)) 
				(forward-sexp))
			(t (message "No enclosing or previous function to select")))))
;; ==========================================================================
(defun newlisp-evaluate-function ()
  "Evaluate the enclosing (or previous) function"
  (interactive)
  (save-excursion
	(let ((found nil))
	  (cond ((newlisp-at-function-startp)
			 (setq found t))
			((newlisp-previous-functionp)
			 (setq found t)))
	  (cond (found 
			  (forward-sexp)
              (newlisp-evaluate-prev-sexp))
			(t (message 
				 "No enclosing or previous function to select for evaluation"))))))
;; ==========================================================================
(defun newlisp-evaluate-buffer()
  "Tells the inferior process to load the current buffer.
   Uses the newlisp 'load command."
  (interactive)
    (process-send-string 
	  newlisp-process-name 
	  (concat "(load \"" (buffer-file-name) "\")\n")))
;; ==========================================================================
;; CONTRIB: fronter000
;; Modified by Maintainer. Let form added.
;; ==========================================================================
(defun newlisp-quote-comments (str)
  "Quote comments for consistant evaluation."
  (let ((idx 0))
    (while (setq idx (string-match ";" str idx))
	  (store-substring str idx ?{)
	  (setq idx (string-match "\n" str idx))
	  (store-substring str idx ?})))
  str)
;; ==========================================================================
;; CONTRIB: frontera000 provided 'newlisp-surround-cmds
;; Maintainer: Wrapped code in 'let form
;; Functionality: Cleaner interpreter window.
;; ==========================================================================
(defun newlisp-evaluate-region (beg end)
  "Send the current region to the inferior newlisp process, removing newlines."
  (interactive "r")
  (let ((str 
          (newlisp-surround-cmds 
            (buffer-substring-no-properties beg end)))) 
    (process-send-string
      newlisp-process-name str)))
;; ==========================================================================
;; CONTRIB: frontera000. Code evaluated, not displayed in interpreter window
;; ==========================================================================
(defun newlisp-surround-cmds (str)
  "Provide 'cmd directive for code"
  (concat "\n[cmd]\n" str "\n[/cmd]\n"))
;; ==========================================================================
(defun newlisp-evaluate-prev-sexp()
  "Send the previous sexp to the inferior Scheme process. 
   Newlines removed."
  (interactive)
  (newlisp-evaluate-region 
	(save-excursion (backward-sexp) (point)) (point)))
;; =====================================================================================
;;  Top-level Values
;; =====================================================================================
(defconst newlisp-binary-name "newlisp" "Process executable")
;; =====================================================================================
(defconst newlisp-process-name "newlisp" "Newlisp Process Name")
;; =====================================================================================
(defconst newlisp-function-regexp 
		  (regexp-opt '("define" "defun" "fn")) 
		  "Newlisp function names")
;; =====================================================================================
(defcustom newlisp-doc-buffer  "*newlisp-doc-buffer*"
  "Unique buffer name for newlisp docs"
  :type  'string
  :group 'newlisp)
;; ==========================================================================
(defvar newlisp-help-buffers
  `("*Help*" ,newlisp-doc-buffer)
  "Can hold any buffer that can get in the way. newlisp-kill-help-buffers
uses this for cleanup.")
;; ==========================================================================
(defcustom newlisp-comment-prefix ";;"
  "*String used by \\[comment-region] to comment out a block of code."
  :type 'string
  :group 'newlisp)
;; ==========================================================================
(defun newlisp-font-lock-fontify-buffer ()
  "Just a wrapper for font-lock-fontify-buffer. Use liberally to refontify
multi-line strings. HINT: put cursor outside of string when using." 
  (interactive)
  (font-lock-fontify-buffer))
;; ==========================================================================
(defun newlisp-previous-functionp ()
  "Look for the preceding function definition. 
Move there and return t if found. 
Reset to starting point and return nil if not found."
  (interactive)
  (let (res (start (point)))
	(setq res 
		  (re-search-backward 
			newlisp-function-begin-regexp nil 'move))
	(cond 
	  (res
		(if (newlisp-at-function-startp) 
		  (setq res t)
		  (goto-char start)
		  (setq res nil)))
	  (t 
		(goto-char start) 
		(setq res nil)))
	res)
  )
;; ==========================================================================
(defun newlisp-next-functionp ()
  "Look for next function definition. 
Move there and return t if found. 
Reset to starting point and return nil if not found."
  (interactive)
  (if (eq 40 (char-after)) (forward-char 1))
  (let (res (start (point)))
	(setq res 
		  (re-search-forward newlisp-function-begin-regexp nil 'move))
	(cond 
	  (res
		(re-search-backward "(")
		(if (newlisp-at-function-startp) 
		  (setq res t)
		  (goto-char start)
		  (setq res nil)))
	  (t (goto-char start) ;; go back to where we started
		 (setq res nil)))
	res))
;; ==========================================================================
(defun newlisp-previous-function()
  "Moves point backwards to the beginning of the nearest function definition"
  (interactive)
  (let (res)
	(setq res (newlisp-previous-functionp))
	(if (not res)
	  (message "No previous function"))))
;; ==========================================================================
(defun newlisp-next-function()
  "Moves point backwards to the beginning of the nearest function definition"
  (interactive)
  (let (res) 
	(setq res (newlisp-next-functionp))
	(if (not res)
	  (message "No function found while searching forward."))))
;; ==========================================================================
(defun newlisp-comment-line ()
  "Comment out line"
  (interactive)
  (save-excursion
  	(back-to-indentation)
  	(insert newlisp-comment-prefix)))
;; ==========================================================================
(defun newlisp-uncomment-line ()
  "Uncomment line"
  (interactive)
  (save-excursion
	(back-to-indentation)
  	(while (eq (char-after) 59)
			 (delete-char 1))))
;; ==========================================================================
(defun newlisp-comment-region (beg end &optional arg)
  "comment out the region."
  (interactive "r\nP")
  (let ((comment-start newlisp-comment-prefix))
    (comment-region beg end arg)))
;; ==========================================================================
(defun newlisp-uncomment-region (beg end &optional arg)
  "Uncomment region."
  (interactive "r\nP")
  (let ((comment-start newlisp-comment-prefix))
    (comment-region beg end -1)))
;; ===============================================================================================
;; Inferior process
;; ===============================================================================================
(defun newlisp-clear-comint-buffer ()
  "Clear the Interpreter input/output window"
  (interactive)
  (newlisp-visit-interpreter)
  (let (begin end)
    (beginning-of-buffer)
    (setq begin (point))
    (end-of-buffer)
    (setq end (point))
    (delete-region begin end)
    (other-window 1)))
;; ===============================================================================================
(defun newlisp-show-interpreter()
  "Start and/or show interpreter in other window.
Cursor stays at point."
  (interactive)
  (switch-to-buffer-other-window 
	(make-comint newlisp-process-name newlisp-binary-name)) 
  (other-window -1))
;; ===============================================================================================
(defun newlisp-visit-interpreter()
  "Start and/or show interpreter in other window.
Then, put cursor in other window."
  (interactive)
  (switch-to-buffer-other-window 
	(make-comint newlisp-process-name newlisp-binary-name)))
;; ==========================================================================
(defun newlisp-indent-line () 
  "Set a line to proper lisp-style indentation.
   Sometimes this means that a line may be `out'dented."
   (interactive) (lisp-indent-line))
;; ==========================================================================
(defun newlisp-indent-sexp() 
  "Set a sexp to proper lisp-style indentation.
   Sometimes this means that a sexp may be `out'dented."
   (interactive) (indent-sexp))
;; ==========================================================================
(defun newlisp-nudge-region ()
  "Indent region by space"
  (interactive "r\nP")
  (indent-rigidly beg end 1)
  (exchange-point-and-mark))
;; =====================================================================
(defun newlisp-tab-region (beg end &optional arg)   
  "Indent a region by a tab."
  (interactive "r\nP")
  (indent-rigidly beg end tab-width)
  (exchange-point-and-mark))
;; ==========================================================================
(defun newlisp-delete-sexp ()
  "Delete outermost enclosing sexp."
  (interactive)
  (cond
	((eq (char-after) 40)   ;; cursor on '('
	 (kill-sexp 1))
	((eq (char-after) 41)   ;; cursor on ')'
	 (forward-char 1) 
	 (backward-sexp)
	 (kill-sexp 1))
	(t (newlisp-sexp-start) ;; find nearest preceding '('
	   (kill-sexp 1))))
;; ==========================================================================
(defun newlisp-context-qualify ()
  "Following convention for context, uppercase and add colon."
    (interactive)
	(if (not (bowp))
		(backward-word 1))
	(upcase-word 1)
	(insert ":"))
;; ==========================================================================
;; CONTRIB: Jeff Ober - allows selection by list and evaluation by list like
;; tuareg mode.
;; ==========================================================================
(defun newlisp-list-open ()
	"As with Emacs Lisp Mode, assumes the nearest opening paren at the first
	position within a line is the root opening paren."
	(interactive)
	(if (not (bobp)) (re-search-backward "^(")))

(defun newlisp-list-close ()
	"Finds the current list's closing paren."
	(interactive)
	(newlisp-sexp-start)
	(forward-char 1)
	(let ((openers 1) (closers 0))
		(while (and (not (eobp)) (> openers closers))
			(cond
				((eq (following-char) ?\() (setq openers (+ 1 openers)))
                                ((eq (following-char) ?\)) (setq closers (+ 1 closers))))
			(forward-char 1))))

(defun newlisp-select-list ()
	"Selects the current list."
	(interactive)
	(beginning-of-line)
	(forward-char 1)
	(newlisp-list-open)
	(set-mark (point))
	(newlisp-list-close))

(defun newlisp-incremental-eval ()
	"Evaluates the current list (rooted at the beginning of a line) and moves
	on to the next (like Tuareg mode)."
	(interactive)
	(newlisp-select-list)
	(let ((b (mark)) (e (point)))
		(deactivate-mark)
		(newlisp-evaluate-region b e)
		(re-search-forward "(")
		(backward-char 1)))
;; ==========================================================================
(defvar newlisp-font-lock-keywords
   `(,@scheme-font-lock-keywords  ;; note: backquote and splice operator!
     ;; add new keywords for highlighting in our sample face
     (,(concat "\\<\\(" newlisp-keywords-regexp "\\)\\>")  ;; builtin keywords + word boundaries
      0 newlisp-font-lock-keywords-face)  ;; removed 't as last argument
     (,(concat "\\<\\(" newlisp-user-keywords-regexp "\\)\\>")  ;; user keywords
      0 newlisp-font-lock-user-keywords-face)
     (,(concat ":\\(" newlisp-user-keywords-regexp "\\)\\>")  ;; user keywords with ':' prefix
      0 newlisp-font-lock-user-keywords-face)
     (,(concat "\\<\\(" newlisp-function-names-regexp "\\)\\>")  ;; function keywords + word boundaries
      0 newlisp-font-lock-function-names-face t)
     ;; Multi-line string highlighting. HINT: use ctrl-c f to refontify
     ;;   NOTE: emacs does not handle multi-line string well in this manner.
     ;;     (JB) suggests looking at how perl and AUCTex handle this.
     ;("[^#]\\({[^{}]*}\\)" 0 'font-lock-string-face) ;; braces, {}
     ("[^#]\\({[^{}]*}\\)" 0 font-lock-string-face t) ; long string
     ("[^#]\\(\\[text\\][^{}]*\\[/text\\]\\)" 0 'font-lock-string-face t) ;; [text] [/text]
     ("'[A-Za-z0-9\-_*0-9]*" 0 'newlisp-font-lock-quote-face)
	 ("\\(^\\|[^\$\\\]\\)#.*" 0 'font-lock-comment-face t) ;; ## comments
	 ("\\(^\\|[^\$\\\]\\);.*" 0 'font-lock-comment-face t) ;; `;;' comments
     )
   "List of newlisp keywords and faces.")
;; ==========================================================================
;; Construct a keymap for the mode.
;; ==========================================================================
(defvar newlisp-mode-map
  (let ((map (make-sparse-keymap))) ;; c-h make-sparse-keymap <RET>
	;; Here we may define any number of key sequences for our mode
	;; c-h define-key <RET>
	(define-key map [(control c) (control b) (s)] 'newlisp-show-interpreter)
	(define-key map [(control c) (control b) (v)] 'newlisp-visit-interpreter)
	(define-key map [(control c) (control b) (c)] 'newlisp-clear-comint-buffer)
	; --------------------------------------------------------------
	(define-key map [(control c) (control e) (b)] 'newlisp-evaluate-buffer)
	(define-key map [(control c) (control e) (p)] 'newlisp-evaluate-prev-sexp)
	(define-key map [(control c) (control e) (r)] 'newlisp-evaluate-region)
	(define-key map [(control c) (control e) (f)] 'newlisp-evaluate-function)
	; -----------------------------------------------------------------------
	(define-key map [(control c) (control i) (n)] 'newlisp-nudge-region)
	(define-key map [(control c) (control i) (t)] 'newlisp-tab-region)
	(define-key map [(control c) (control i) (l)] 'newlisp-indent-line)
	(define-key map [(control c) (control i) (x)] 'newlisp-indent-sexp)
	(define-key map [(control c) (control i) (d)] 'newlisp-delete-sexp)
	(define-key map [(control c) (control i) (\;)] 'newlisp-context-qualify)
	; -----------------------------------------------------------------------
	(define-key map [(control c) (control n)] 'newlisp-next-function)
	(define-key map [(control c) (control p)] 'newlisp-previous-function)
	(define-key map [(control c) (?\[)] 'newlisp-sexp-start)
	(define-key map [(control c) (?\])] 'newlisp-sexp-end)
	(define-key map [(control c) (control ?\[)] 'forward-sexp)  ;; note: menu and help view will show C-c ESC
	(define-key map [(control c) (control ?\])] 'backward-sexp)
	; -----------------------------------------------------------------------
	(define-key map [(control c) (control s) (x)] 'newlisp-select-sexp)
	(define-key map [(control c) (control s) (s)] 'newlisp-select-function)
	(define-key map [(control c) (control c) (c)] 'newlisp-comment-region)
	(define-key map [(control c) (control c) (\;)] 'newlisp-comment-line)
	(define-key map [(control c) (control c) (control \;)] 'newlisp-uncomment-line)
	(define-key map [(control c) (control c) (u)] 'newlisp-uncomment-region)
	(define-key map [(control c) (control f)] 'newlisp-font-lock-fontify-buffer)
	; -----------------------------------------------------------------------
	; 2008-03-12 Jeff Ober: a few more simple shortcuts
	; -----------------------------------------------------------------------
	(define-key map [(control x) (control e)] 'newlisp-incremental-eval)
	; -----------------------------------------------------------------------
	map)
  "Keymap for `newlisp-mode'.")
;; ==========================================================================
;; Define the menu using 'easy-menu-define for
;; best compatibility for both forks.
;; ==========================================================================
(easy-menu-define    ;; c-h f easy-menu-define <RET>
  newlisp-menu newlisp-mode-map "Newlisp Mode Menu"
  '("Newlisp" 
	["Show Interpreter" newlisp-show-interpreter]
	["Visit Interpreter" newlisp-visit-interpreter]
	["Clear Interpreter" newlisp-clear-comint-buffer]
	["Evaluate Buffer" newlisp-evaluate-buffer]
	["Evaluate Region" newlisp-evaluate-region]
	["Evaluate Prev Sexp" newlisp-evaluate-prev-sexp]
	["Evaluate Function" newlisp-evaluate-function]
	"-" ;; seperator
	("Text Operations"  ;; submenu
	 ["Indent Region by TAB" newlisp-tab-region]
	 ["Indent Region by SPACE" newlisp-nudge-region]
	 ["Indentation for Line" newlisp-indent-line]
	 ["Indent Sexp" newlisp-indent-sexp]
	 ["Delete Sexp" newlisp-delete-sexp]
	 ["Select Sexp" newlisp-select-sexp]
	 ["Context" newlisp-context-qualify]
	 )
	"-" ;; seperator
	["Next function" newlisp-next-function]
	["Previous function" newlisp-previous-function]
	["Nearest Start of Sexp" newlisp-sexp-start]
	["Nearest End of Sexp" newlisp-sexp-end]
	["Forward Sexp" forward-sexp]
	["Backward Sexp" backward-sexp]
	"-" ;; seperator
	["Select function" newlisp-select-function]
	["Select Sexp" newlisp-select-sexp]
	["Comment Out Region" newlisp-comment-region]
	["Comment Out Line" newlisp-comment-line]
	["Uncomment Region" newlisp-uncomment-region]
	["Uncomment Line" newlisp-uncomment-line]
	["Fontify Buffer" newlisp-font-lock-fontify-buffer]
	))
;; ==========================================================================
(define-derived-mode newlisp-mode scheme-mode "newlisp"
  "A major mode for Newlisp."
  (easy-menu-add newlisp-menu) ;; install main menu
  (imenu-add-menubar-index)    ;; install imenu with title "Index"
  (setq imenu-sort-function 'imenu--sort-by-name)  ;; alternatively: 'imenu--sort-by-position
  (setq auto-rescan t)         ;; tell imenu to rescan every time it is used
  (run-hooks 'newlisp-mode-hook)
  (message "Load Newlisp Mode")
  ;; Highly Recommended: c-h v font-lock-keywords <RET>
  (set (make-local-variable 'font-lock-defaults)
       (cons 'newlisp-font-lock-keywords
             (or (cdr font-lock-defaults)
                 '(nil t ;; syntax table modifications follow: You may wish to use
                    ;; For help: C-h f modify-syntax-entry <RET>
                    ;; Bind non-alpha characters to the 'word' syntax class
                       ((?+ . "w") (?- . "w") (?* . "w") (?/ . "w")
                        (?. . "w") (?< . "w") (?> . "w") (?= . "w")
                        (?? . "w") (?$ . "w") (?% . "w") (?_ . "w")
                        ;(?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))))))
                        (?& . "w") (?~ . "w") (?^ . "w") )))))
                        ;; NOTE: Emacs accepts a more compact approach.
                        ;; The cons-cell list approach used here is for XEmacs compatibility.
						(define-key scheme-mode-map [menu-bar scheme] nil)  ;; drop the scheme menu
						)
;;; newlisp.el ends here
