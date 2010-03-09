;;; tj-parenface.el --- Provide a face for parens and square brackets in
;;  numerous modes.
;; Based on parenface By Dave Pearson <davep@davep.org>
;;   Bracket support and additional hookds added by 
;;   Tim Johnson <tim@johnsons-web.com>
;;; Credits:
;;    Thanks to Dave Pearson who indicates that his code was
;;    based on some code that Boris Schaefer <boris@uncommon-sense.net> posted
;;    to comp.lang.scheme in message <87hf8g9nw5.fsf@qiwi.uncommon-sense.net>.
;; 
;; $Revision: 1.1 $

;; Add a paren-face to emacs and add support for it to various modes.
;; Add a bracket-face to emacs and add support for it to various modes.
;;
(defvar paren-face 'paren-face)
(defvar bracket-face 'bracket-face)
(defvar brace-face 'brace-face)
(defvar fslash-face 'fslash-face)
(defface fslash-face 
    '((((class color)) (background dark)(:foreground "white"))
	  (((class color)) (background light)(:foreground "blue")))
  "Face for displaying a forward slash."
  :group 'faces)
(defface paren-face
    '((((class color)) (background dark)(:foreground "white"))
	  (((class color)) (background light)(:foreground "blue")))
  "Face for displaying a paren."
  :group 'faces)
(defface bracket-face
    '((((class color)) (background dark)(:foreground "yellow2"))
	  (((class color)) (background light)(:foreground "#8B6508")))
  "Face for displaying a square bracket."
  :group 'faces)
(defface brace-face
    '((((class color)) (background dark)(:foreground "yellow2"))
	  (((class color)) (background light)(:foreground "#8B6508")))
  "Face for displaying braces."
  :group 'faces)
(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "(\\|)")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) paren-face)
        (setq ,keywords (append (list (cons regexp paren-face)) ,keywords))))))
(defmacro bracket-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "\\[\\|\\]")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) bracket-face)
        (setq ,keywords (append (list (cons regexp bracket-face)) ,keywords))))))
(defmacro brace-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((regexp "{\\|}")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) brace-face)
        (setq ,keywords (append (list (cons regexp brace-face)) ,keywords))))))
;(defmacro fslash-face-add-support (keywords)
;  "Generate a lambda expression for use in a hook."
;  `(lambda ()
;    (let* ((regexp "\\/")
;           (match (assoc regexp ,keywords)))
;      (unless (eq (cdr match) bracket-face)
;        (setq ,keywords (append (list (cons regexp fslash-face)) ,keywords))))))
;; Keep the compiler quiet.
(eval-when-compile
  (defvar scheme-font-lock-keywords-2 nil)
  (defvar lisp-font-lock-keywords-2 nil))

(add-hook 'scheme-mode-hook           (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'newlisp-mode-hook          (paren-face-add-support newlisp-font-lock-keywords))
(add-hook 'lisp-mode-hook             (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp1-mode-hook            (paren-face-add-support lisp1-font-lock-keywords))
(add-hook 'rebol-mode-hook            (bracket-face-add-support rebol-font-lock-keywords))
(add-hook 'rebol-mode-hook            (paren-face-add-support rebol-font-lock-keywords))
(add-hook 'tj-lua-mode-hook            (bracket-face-add-support tj-lua-font-lock-keywords))
(add-hook 'tj-lua-mode-hook            (paren-face-add-support tj-lua-font-lock-keywords))
(add-hook 'tj-lua-mode-hook            (brace-face-add-support tj-lua-font-lock-keywords))
;(add-hook 'rebol-mode-hook            (fslash-face-add-support rebol-font-lock-keywords))
(add-hook 'python-mode-hook           (paren-face-add-support python-font-lock-keywords))
(add-hook 'emacs-lisp-mode-hook       (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))

(provide 'tj-parenface)

;; tj-parenface.el ends here
