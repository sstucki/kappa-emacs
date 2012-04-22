;;; kappa.el -- major mode for Kappa models

;; Copyright (C) 2012 Sandro Stucki

;; Authors:
;;   Sandro Stucki <sandro.stucki@ed.ac.uk>


;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A major mode for editing Kappa models.  The mode knows enough about
;; Kappa syntax to do some basic fontification but does currently not
;; do indentation or proper slashification.

;; There are numerous font face customization variables.


;;; Known bugs/limitations:

;; * There seems to be a bug that cause some version of Emacs to
;;   fontify the entire buffer with font-lock-comment-face.


;;; To Do:

;; * Fix comment font-lock weirdness.
;; * Support for indentation and slashification.  Hard.
;; * Add function for running KaSim automatically in a separate
;;   buffer.
;; * Documentation.



;;; Customization stuff

;; Customization group for Kappa mode.
(defgroup kappa nil
  "Kappa mode customization."
  :group 'languages)


;; Customization of fontification

(defface kappa-keyword-face
  '((t :inherit font-lock-preprocessor-face))
  "Face to use for highlighting Kappa keywords such as \"%var\"
or \"%init\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-command-face
  '((t :inherit font-lock-keyword-face))
  "Face to use for highlighting Kappa commands such as \"$ADD\"
or \"$STOP\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-rule-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa rule operators \"@\"
and \"->\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-math-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting algebraic and logic operators
such as \"+\" or \"/\" in Kappa expressions in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-interface-symbol-face
  '((t :inherit font-lock-builtin-face))
  "Face to use for highlighting the Kappa agent interface symbols
such as \"!\" or \"~\" in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-builtin-face
  '((t :inherit font-lock-constant-face))
  "Face to use for highlighting built-in functions such as
\"[sin]\" or \"[mod]\" in Kappa expressions in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-constant-face
  '((t :inherit font-lock-constant-face))
  "Face to use for highlighting numerical and logical constants
such as \"[pi]\" or \"[true]\" in Kappa expressions in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-agent-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to use for highlighting Kappa agent names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-site-name-face
  '((t nil))
  "Face to use for highlighting Kappa site names in Font-Lock
mode."
  :group 'faces
  :group 'kappa)

(defface kappa-link-label-face
  '((t :inherit font-lock-type-face))
  "Face to use for highlighting the labels of links between Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-internal-state-face
  '((t :inherit font-lock-variable-name-face))
  "Face to use for highlighting the internal state names of Kappa
sites in Font-Lock mode."
  :group 'faces
  :group 'kappa)

(defface kappa-string-face
  '((t :inherit font-lock-string-face))
  "Face to use for highlighting string literals such as Kappa
variables in Font-Lock mode."
  :group 'faces
  :group 'kappa)


;; Variable definitions from face definitions
(defvar kappa-keyword-face 'kappa-keyword-face
  "Face for highlighting Kappa keywords.")
(defvar kappa-command-face 'kappa-command-face
  "Face for highlighting Kappa commands.")
(defvar kappa-rule-operator-face 'kappa-rule-operator-face
  "Face for highlighting Kappa rule operators.")
(defvar kappa-math-operator-face 'kappa-math-operator-face
  "Face for highlighting math operators in Kappa mode.")
(defvar kappa-interface-symbol-face 'kappa-interface-symbol-face
  "Face for highlighting Kappa agent interface symbols.")
(defvar kappa-builtin-face 'kappa-builtin-face
  "Face for highlighting built-in functions in Kappa mode.")
(defvar kappa-constant-face 'kappa-constant-face
  "Face for highlighting constants in Kappa mode.")
(defvar kappa-agent-name-face 'kappa-agent-name-face
  "Face for highlighting Kappa agent names.")
(defvar kappa-site-name-face 'kappa-site-name-face
  "Face for highlighting Kappa site names.")
(defvar kappa-link-label-face 'kappa-link-label-face
  "Face for highlighting Kappa link labels.")
(defvar kappa-internal-state-face 'kappa-internal-state-face
  "Face for highlighting Kappa internal state names.")
(defvar kappa-string-face 'kappa-string-face
  "Face for highlighting string literals in Kappa mode.")


;;; Main Kappa mode definition (using generic)

(define-generic-mode kappa-mode

  '(?#)      ;; Comments start with '#'

  nil        ;; Handle keywords using font-lock rules (see below)

  ;; Rules for syntax highlighting (font-lock)
  (eval-when-compile
    (let
        ;; Common lexical sub-expressions used in keywords
        ((id "[A-Za-z0-9][A-Za-z0-9_-]*")   ;; IDs/names as defined in
                                            ;; the Kappa spec
         (idx "[A-Za-z][A-Za-z0-9_-]*")     ;; IDs/names without the
                                            ;; initial digits
         (num "[0-9]+")                     ;; Integer numerals
         (ws "[ \t]*"))                     ;; Whitespace

      (append

       ;; Keywords
       (kappa-make-font-lock-keyword-list
        '("^%agent:" "^%var:" "^%plot:" "^%obs:" "^%init:" "^%mod:")
        kappa-keyword-face)

       ;; Commands
       (kappa-make-font-lock-keyword-list
        '("$ADD" "$DEL" "$SNAPSHOT" "$STOP")
        kappa-command-face)

       ;; Built-in functions
       (kappa-make-font-lock-keyword-list
        '("\\[not\\]" "\\[log\\]" "\\[sin\\]" "\\[cos\\]" "\\[tan\\]"
          "\\[sqrt\\]" "\\[mod\\]" "\\[exp\\]" "\\[int\\]")
        kappa-builtin-face)

       ;; Symbolic numerical constants
       (kappa-make-font-lock-keyword-list
        '("\\[E\\]" "\\[T\\]" "\\[inf\\]" "\\[pi\\]" "\\[emax\\]"
          "\\[tmax\\]" "\\[true\\]" "\\[false\\]")
        kappa-constant-face)

       (list

        ;; Agent interface symbols
        '("[?!~]" . kappa-interface-symbol-face)

        ;; Internal state names
        (list (concat "~" ws "\\(" id "\\)")
              1 kappa-internal-state-face)

        ;; Link labels
        (list (concat "!" ws "\\(" num          ;; Numeric label
                      "\\|" id ws "\\." ws id   ;; Remote site name
                      "\\|" ws "_\\)")          ;; Wildcard
              1 kappa-link-label-face)

        ;; Variable names
        '("'[^'\n]+'" . kappa-string-face)

        ;; Agent names followed by an interface spec and site names
        (list (concat "\\(" id "\\)" ws "(")      ;; Agents
              '(1 kappa-agent-name-face)
              (list                                ;; Site interface
               (concat "\\=" ws "\\(" id "\\)[^,)\n]*"
                       "\\(," ws "\\([^A-Za-z0-9,)\n][^,)\n]*\\)?\\)*")
               nil nil
               '(1 kappa-site-name-face)))

        ;; Numerals
        (cons (concat num "\\|\\(" num "\\)?\\." num
                      "\\([Ee][+-]?" num "\\)?")
              kappa-constant-face)

        ;; Agent names not followed by an interface spec (all
        ;; remaining IDs without initial digits)
        (cons idx kappa-agent-name-face)
        ;; '("\\.\\.\\." . kappa-agent-name-face)
        )

       ;; Rule operators
       (kappa-make-font-lock-keyword-list
        '("@" "->")
        kappa-rule-operator-face)

       ;; Math operators
       (kappa-make-font-lock-keyword-list
        '("&&" "||" "[+*/^:=<>-]")
        kappa-math-operator-face))))

  ;; File suffixes for which to activate this mode 
  '("\\.ka\\'")

  nil   ;; other hooks to call

  "Major mode for editing Kappa models.

Turning on Kappa mode runs the hook `kappa-mode-hook'.")


;;; Helper functions

(defun kappa-make-font-lock-keyword-list (keywords face)
  "Make a font-lock keyword list containing a matching each
keyword in the list `keywords' to the face spec `face'."
  (mapcar (lambda (k) (cons k face)) keywords))

(provide 'kappa)

;;; kappa.el ends here
