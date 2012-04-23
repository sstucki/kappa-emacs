;;; kappa.el -- major mode for Kappa models

;; Copyright (C) 2012 Sandro Stucki, Sebastian Jaramillo,
;;                    Ricardo Honorato-Zimmer

;; Authors:
;;   Sandro Stucki <sandro.stucki@ed.ac.uk>
;;   Sebastian Jaramillo <sebajarar@gmail.com>
;;   Ricardo Honorato-Zimmer <rikardo.horo@gmail.com>


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
;; * Support for indentation and slashification. Hard.
;; * Documentation.
;; * Remove redundant (?) font-lock variables.
;; * Remove dependency on sed(1).


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


;; Variable definitions from face definitions (FIXME: Redundant?)
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


;;; KaSim related customization variables

(defcustom kappa-KaSim-executable-path "/usr/bin/KaSim"
  "File system path to the KaSim executable."
  :type 'file
  :group 'kappa)

(defcustom kappa-default-KaSim-time 200
  "Default KaSim simulation duration."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-KaSim-events 10000
  "Default number of KaSim events to produce per simulation."
  :type 'number
  :group 'kappa)

(defcustom kappa-default-KaSim-points 500
  "Default number of KaSim points to produce per simulation."
  :type 'number
  :group 'kappa)

;; Variables to remember the values of the arguments of previous
;; invocation of `kappa-run-KaSim-job' and `kappa-plot-KaSim'.
(defvar kappa-prev-KaSim-output ""
  "Value of the `output' or `file-path' argument during the
  previous invocation of `kappa-run-KaSim-job' or
  `kappa-plot-KaSim', respectively.")
(defvar kappa-prev-KaSim-time kappa-default-KaSim-time
  "Value of the `time' argument during the previous invocation of
  `kappa-run-KaSim-job'.")
(defvar kappa-prev-KaSim-events kappa-default-KaSim-events
  "Value of the `events' argument during the previous invocation
  of `kappa-run-KaSim-job'.")
(defvar kappa-prev-KaSim-points kappa-default-KaSim-points
  "Value of the `points' argument during the previous invocation
  of `kappa-run-KaSim-job'.")
(defvar kappa-prev-plot-columns "1"
  "Value of the `columns' argument during the previous invocation
  of `kappa-plot-KaSim'.")


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
         (num "[0-9]+")                     ;; Integer numerals
         (ws "[ \t]*"))                     ;; Whitespace

      (list

       ;; Keywords
       (cons
        (regexp-opt
         '("%agent:" "%var:" "%plot:" "%obs:" "%init:" "%mod:"))
        kappa-keyword-face)

       ;; Commands
       (cons
        (regexp-opt
         '("$ADD" "$DEL" "$SNAPSHOT" "$STOP"))
        kappa-command-face)

       ;; Built-in functions
       (cons
        (regexp-opt
         '("[not]" "[log]" "[sin]" "[cos]" "[tan]" "[sqrt]" "[mod]"
           "[exp]" "[int]"))
        kappa-builtin-face)

       ;; Symbolic numerical constants
       (cons
        (regexp-opt
         '("[E]" "[T]" "[inf]" "[pi]" "[emax]" "[tmax]" "[true]"
           "[false]"))
        kappa-constant-face)

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
       (cons (concat "\\<\\(\\(" num "\\)?\\." num "\\([Ee][+-]?" num
                     "\\)?\\|" num "\\)\\>")
             kappa-constant-face)

       ;; Agent names not followed by an interface spec (all
       ;; remaining IDs)
       (cons id kappa-agent-name-face)
       ;; '("\\.\\.\\." . kappa-agent-name-face)

       ;; Rule operators
       '("@\\|->" . kappa-rule-operator-face)

       ;; Math operators
       '("&&\\|||\\|[+*/^:=<>-]" . kappa-math-operator-face))))

  ;; File suffixes for which to activate this mode 
  '("\\.ka\\'")

  nil   ;; other hooks to call

  "Major mode for editing Kappa models.

Turning on Kappa mode runs the hook `kappa-mode-hook'.
")


;;; KaSim related functions.

(defvar *buffer-counter* 1)

(defun kappa-run-KaSim-job (input output time events points)
  "Input:
  INPUT: File path to the Kappa model.
  OUTPUT: Where KaSim will print the results.
  TIME: Time (integer). Default is the value of
        `kappa-default-KaSim-time'.
  EVENTS: Number of events (integer). Default is the value of
          `kappa-default-KaSim-events'.
  POINTS: Number of points (integer). Default is the value of
          `kappa-default-KaSim-points'.

Output: none.

Side Effects: Creates *KaSim* buffer, print the arguments passed
to *Messages* and run `kappa-KaSim-executable-path' in shell with
the appropriate arguments.

Related variables: `kappa-KaSim-executable-path',
`kappa-default-KaSim-time', `kappa-default-KaSim-events',
`kappa-default-KaSim-points'.
"
  (interactive
    (list (read-file-name "Input: " (file-truename buffer-file-name))
          (read-file-name "Output: " kappa-prev-KaSim-output)
          (read-number "Time: " kappa-prev-KaSim-time)
          (read-number "Events: " kappa-prev-KaSim-events)
          (read-number "Points: " kappa-prev-KaSim-points)))

  (setq kappa-prev-KaSim-output output)
  (setq kappa-prev-KaSim-time time)
  (setq kappa-prev-KaSim-events events)
  (setq kappa-prev-KaSim-points points)

  ;; FIXME: Would be nice to eliminate the dependency on sed(1).
  (let ((command (concat kappa-KaSim-executable-path " -i " input
                         " -o " output
                         (cond
                           ((> time 0)   (format " -t %s" time))
                           ((> events 0) (format " -e %s" events)))
                         (when points
                           (format " -p %s" points))
                         " && sed -i s/^#// " output))
        (buffer-name (concat "*KaSim (" (car (last (split-string input "/")))
			     ") " (number-to-string *buffer-counter*) " *")))

    (when (file-exists-p output)
          (if (y-or-n-p (concat "File '" output "' exists. Would you like to "
				"delete it to run the simulation?"))
              (delete-file output)
              (error "%s" (concat "Output file " output " has not been "
				  "overwritten"))))

    (message "KaSim command executed: %s\n" command) ; save the command to *Message* buffer
    (shell-command command (get-buffer-create buffer-name))

    (setq *buffer-counter* (+ 1 *buffer-counter*))
    (format (concat "Done! See " buffer-name " buffer for details"))))


;;; Gnuplot related functions.

(defun kappa-plot-KaSim (&optional file-path columns)
  "Simple function for plotting a file.

  FILE-PATH is the full path to a file that can be read by
            gnuplot.  The first row is expected to contain the
            headers for each column.

  COLUMNS is a string containing the columns to be ploted
          separated by space.

By default the following options would be set in gnuplot:
autoscale, xtic auto, ytic auto, key autotitle columnhead,
ylabel \"Number of Molecules\", xlabel \"Time\"

This function requires the installation of gnuplot-mode. You can
find it at

  https://github.com/bruceravel/gnuplot-mode/
"

  (interactive
   (list (read-file-name "KaSim's output file: "
                          kappa-prev-KaSim-output)
         (read-string "Columns separated by space: "
                      kappa-prev-plot-columns)))

  ;; This part requires the installation of gnuplot-mode!
  ;; https://github.com/bruceravel/gnuplot-mode/
  (if (not (require 'gnuplot nil t))
      (error "Could not find Gnuplot mode! Gnuplot mode is \
required for plotting.")

    (setq kappa-prev-KaSim-output file-path)
    (setq kappa-prev-plot-columns columns)

    (gnuplot-send-string-to-gnuplot
     (concat "set autoscale\n"
             "set xtic auto\n"
             "set ytic auto\n"
             "set key autotitle columnhead\n"
             "set ylabel \"Number of Molecules\"\n"
             "set xlabel \"Time\"\n"
             "set title \"" (car (last (split-string file-path "/"))) "\"\n"
             "plot "
             (let ((cols (mapcar (lambda (x)
                                   (+ 2 (string-to-number x)))
                                 (split-string columns))))
               (mapconcat (lambda (n)
                            (concat "\'" file-path "\' using 1:"
                                    (number-to-string n) " with lines"))
                          cols ", ")) "\n")
     nil)))


(provide 'kappa)

;;; kappa.el ends here
